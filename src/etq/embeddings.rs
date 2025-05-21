use anyhow::Context;
use derive_more::{AsMut, AsRef, Deref, DerefMut, Display, From, Into, IntoIterator};
use linfa::prelude::*;
use linfa_clustering::Dbscan;

// use linfa::dataset::DatasetBase;
use ndarray::prelude::*;
use rust_bert::pipelines::sentence_embeddings;
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    fs::{self, File},
    iter::zip,
    path::PathBuf,
};
use strum::{EnumString, IntoStaticStr};
use tch::Device;
use thiserror::Error;

use anyhow::{Result, anyhow};

use crate::config;

use super::model::{Tag, Tags};

/// Given a tag and a model, compute the cache path to which it can be saved, or
/// from which it could be loaded.
pub fn make_embeddings_cache_path(
    tag: &Tag,
    model: SentenceEmbeddingsModelType,
) -> Result<PathBuf> {
    let subpath: PathBuf = model.to_string().into();
    let subpath = config::get_embeddings_cache_path(Some(subpath))?;
    Ok(subpath.join(tag.normalize()))
}

/// An error encountered during embeddings load, generation, or save.
#[derive(Error, Debug)]
pub enum EmbeddingsError<'a> {
    /// There was a problem loading the embeddings. Note, these _will_ be expected
    /// when we just haven't cached anything.
    #[error("Unable to load embeddings for {0}, model {1}.")]
    Load(
        &'a Tag,
        SentenceEmbeddingsModelType,
        #[source] anyhow::Error,
    ),

    /// There was a problem generating embeddings. These are generally fatal, as
    /// they most likely mean a problem with the underlying model configuration
    /// We can quibble about whether these merit a panic, but the invariants
    /// expected by the underlying library are Important and Mysterious.
    /// For now, we'll catch them and at least wrap them in context.
    #[error("Unable to generate embeddings for {0:?}, model {1}, device {2:?}.")]
    Generate(
        Tags,
        SentenceEmbeddingsModelType,
        Device,
        #[source] anyhow::Error,
    ),

    /// There was a problem saving the embeddings for the given tag to the
    /// cache. We're treating these as fatal for now. In reality, it may be worth
    /// considring these "recoverable", as if we dont save embeddings then we'll just try
    /// again the next time we see them.
    #[error("Unable to save embeddings to {2:?} for {0}, model {1}: {3:?}.")]
    Save(
        Tag,
        SentenceEmbeddingsModelType,
        Option<PathBuf>,
        #[source] anyhow::Error,
    ),

    /// We encountered one or more errors in when loading-or-generating. The vec
    /// members will actually be one of the above, or [`Unknown(e)`].
    #[error("Nonzero errors found trying to load or generate embeddings.")]
    LoadOrGenerate(Box<Vec<EmbeddingsError<'a>>>),

    /// Something happened that the ring did not expect. At the moment, this probably
    /// means a model error; whatever the cause, it's included.
    #[error("Unknown embeddings error.")]
    Unknown(#[from] anyhow::Error),
}

/// Attempt to save the given embeddings.
fn try_save_embeddings<'a>(
    embeddings: TagEmbeddings,
) -> Result<TagEmbeddings, EmbeddingsError<'a>> {
    let path = make_embeddings_cache_path(&embeddings.tag, embeddings.model)
        .with_context(|| "try_save_embeddings(): make embeddings path")
        .map_err(|e| EmbeddingsError::Save(embeddings.tag.clone(), embeddings.model, None, e))?
        .with_extension("cbor");

    let writer = File::options()
        .write(true)
        .create(true)
        .open(&path)
        .with_context(|| "try_save_embeddings(): open for write")
        .map_err(|e| {
            EmbeddingsError::Save(
                embeddings.tag.clone(),
                embeddings.model,
                Some(path.clone()),
                e.into(),
            )
        })?;

    ciborium::into_writer(&embeddings, writer)
        .with_context(|| "try_save_embeddings(): write cbor")
        .map_err(|e| {
            EmbeddingsError::Save(
                embeddings.tag.clone(),
                embeddings.model,
                Some(path.clone()),
                e.into(),
            )
        })?;

    Ok(embeddings)
}

/// Given a tag and a model, attempt to load the associated embeddings from cache.
fn try_load_embeddings<'a>(
    tag: &'a Tag,
    model: SentenceEmbeddingsModelType,
) -> std::result::Result<TagEmbeddings, EmbeddingsError<'a>> {
    let path = make_embeddings_cache_path(tag, model)
        .with_context(|| "try_load_embeddings: make cache path")
        .map_err(|e| EmbeddingsError::Load(tag, model, e.into()))?
        .with_extension("cbor");

    let content = fs::read(&path)
        .with_context(|| "try_load_embeddings(): read data")
        .map_err(|e| EmbeddingsError::Load(tag, model, e.into()))?;

    ciborium::from_reader(content.as_slice())
        .with_context(|| "try_load_embeddings(): decode cbor")
        .map_err(|e| EmbeddingsError::Load(tag, model, e.into()))
}

/// Given a collection of tags, a model, and a device, attempt to load the embeddings from cache,
/// and for any that aren't cached, generated and cache them prior to returning.
/// Embeddings are guaranteed to be in the same order as the tags whence they
/// were generated.
pub fn load_or_generate_embeddings<'a>(
    tags: Tags,
    model: SentenceEmbeddingsModelType,
    device: Device,
) -> std::result::Result<Vec<TagEmbeddings>, EmbeddingsError<'a>> {
    /*
    The following monstrosity:
        1. First, loop through the gags and attempt to load values from disk. Could be concurrent
           but likely not a huge savings.
        2. Collect the results, and for the failures, generate embeddings.
        3. Save the newly generated embeddings
        4. Coalesce the reified and generated embeddings into a single `Embeddings` instance
        5. Return it.
    */

    // First, try to load embeddings
    let (cached_embeddings, errors): (Vec<_>, Vec<_>) = tags
        .iter()
        .map(|t| try_load_embeddings(t, model))
        .partition(|r| r.is_ok());

    // Now, collect all of the tags that errored.
    let tags_to_generate = errors
        .into_iter()
        .filter_map(|f| match f {
            Err(EmbeddingsError::Load(tag, _, _)) => Some(tag),
            _ => None,
        })
        .collect::<Vec<_>>();

    println!("tags_to_generate: {:#?}", tags_to_generate);

    // Attempt to generate and save embeddings. The generation happens
    // all at once, so it's all-or nothing. However we still save
    // one-by-one, which means that we need to first see if we have
    // any errors and fail fast if we do, otherwise return the reults.
    let (generated_embeddings, errors): (Vec<_>, Vec<_>) = generate_embeddings(
        &tags_to_generate
            .iter()
            .map(|t| t.normalize())
            .collect::<Vec<_>>(),
        device,
        model,
    )
    // For each of these that failed, try to generate embeddings
    .map(|em| {
        zip(tags_to_generate, em)
            .into_iter()
            .map(|p| TagEmbeddings {
                tag: p.0.to_owned(),
                model: model,
                embeddings: p.1.into(),
            })
            .map(|t| try_save_embeddings(t))
            .partition(|e| e.is_ok())
    })?;

    match (generated_embeddings, errors) {
        (_, e) if e.len() > 0 => Err(EmbeddingsError::LoadOrGenerate(Box::new(
            e.into_iter().filter_map(|f| f.err()).collect(),
        ))),
        (emb, _) => Ok(cached_embeddings
            .into_iter()
            .filter_map(|f| f.ok())
            .chain(emb.into_iter().filter_map(|f| f.ok()).map(|f| f.to_owned()))
            .collect()),
    }
}

/// Generate embeddings for a slice of things that can be turned into strs.
/// - [`tags`] tags (which may be tags, or may be other things) for which the embeddings
///   should be generated.
/// - [`device`] the GPU device to use.
/// - [`model_type`] The model type to use. Note that this is a mirror of the
///   [`rust_bert::pipelines::sentence_embeddings`] model type, instead of the
///   real thing, because as a foreign enum the latter doesn't play nicely with
///   the type system.
pub fn generate_embeddings<'a, S>(
    tags: &[S],
    device: Device,
    model_type: SentenceEmbeddingsModelType,
) -> std::result::Result<Embeddings, EmbeddingsError<'a>>
where
    S: AsRef<str> + Send + Sync,
{
    if tags.is_empty() {
        Ok(Embeddings::default())
    } else {
        let model = sentence_embeddings::SentenceEmbeddingsBuilder::remote(model_type.into())
            .with_device(device)
            .create_model()
            .expect("Failed to load embedding model");

        model
            .encode(tags)
            .map(|e| e.into())
            .with_context(|| "generate_embedding()/encode")
            .map_err(|e| e.into())
    }
}

/// Given a [`Tags`] collection, calculate embeddings using the given [`SentenceEmbeddingsModelType`] executing
/// on the given [`Device`], cluster using `DBSCAN` with the given `min_points` and `tolerance` hyperparameters.
/// - `tags`: The [`Tags`] instance whose contents should be clustered.
/// - `device`: The GPU device to use for embeddings.
/// - `model_type`: The [`SentenceEmbeddingsModelType`] to use for embeddings.
/// - `min_tags_per_cluster`: The minimum number of tags a cluster can have.
/// - `tolerance`: The `tolerance` hyperparameter to the `DBSCAN` algorithm.
pub fn embed_and_cluster_tags(
    tags: Tags,
    device: Device,
    model_type: SentenceEmbeddingsModelType,
    min_tags_per_cluster: ClusterSize,
    tolerance: DbscanEpsilon,
) -> Result<ClusteringProduct> {
    // let normalized_tags = tags.iter().map(|t| t.normalize()).collect::<Vec<String>>();
    // let embeddings = generate_embeddings(&normalized_tags, device, model_type)
    //     .with_context(|| "embed_and_cluster_tags()")?;
    let embeddings = load_or_generate_embeddings(tags, model_type, device)
        .with_context(|| "embed_and_cluster_tags()")
        .map_err(|e| anyhow::Error::from(e))?;
    let clusters = cluster_tag_embeddings(embeddings, min_tags_per_cluster, tolerance)
        .with_context(|| "embed_and_cluster_tags()")?;

    let result = ClusteringProduct {
        provenance: ClusterProvenance {
            cluster_algorithm: ClusterAlgorithmProvenance::Dbscan(min_tags_per_cluster, tolerance),
            embeddings_model: EmbeddingsProvenance::SentenceEmbeddings(model_type),
        },
        clusters: clusters,
    };
    Ok(result)
}

fn cluster_tag_embeddings(
    tag_embeddings: Vec<TagEmbeddings>,
    min_points: ClusterSize,
    tolerance: DbscanEpsilon,
) -> Result<TagClusters> {
    let (tags, embeddings) = tag_embeddings
        .into_iter()
        .fold((vec![], vec![]), |mut acc, nxt| {
            acc.0.push(nxt.tag);
            acc.1.push(nxt.embeddings);
            acc
        });

    let clusters = cluster_embeddings(&embeddings.into(), min_points, tolerance)?;
    Ok((Tags::from(tags), clusters).into())
}

/// Cluster embeddings using DBSCAN
/// See: https://rust-ml.github.io/book/4_dbscan.html
fn cluster_embeddings(
    embeddings: &Embeddings,
    min_points: ClusterSize,
    tolerance: DbscanEpsilon,
) -> Result<Clusters> {
    let reshaped_embeddings = embeddings
        .as_ndarray()
        .with_context(|| "cluster_embeddings()")?;

    let cluster_assignments = Dbscan::params(min_points.into())
        .tolerance(tolerance.into())
        .transform(&reshaped_embeddings)
        .with_context(|| "cluster_embeddings()")?;

    let mut clusters = Clusters::default();
    for (point_idx, cluster_id) in cluster_assignments.iter().enumerate() {
        clusters.add_cluster_assignment(
            point_idx,
            cluster_id.to_owned().unwrap_or(usize::MAX).into(),
        );
    }
    Ok(clusters)
}

// ========================================================================
// Types
//
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TagEmbeddings {
    tag: Tag,
    model: SentenceEmbeddingsModelType,
    embeddings: Embeddings,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash, From, Into, Display)]
#[display("{{ rows:{} columns:{} }}", self.0.0, self.0.1)]
struct Shape((usize, usize));

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, From, Into, IntoIterator)]
struct FlattenedEmbeddings(Vec<f32>);

/// Newtype wrapper around Embeddings.
/// Wraps a [`Vec<Vec<f32>>`] for type safety, and associates various utility methods.
#[derive(Debug, Clone, From, Serialize, Deserialize, IntoIterator, PartialEq, Default)]
#[serde(transparent)]
pub struct Embeddings(Vec<Vec<f32>>);
impl Embeddings {
    /// The shape of this Embeddings instance.
    fn shape(&self) -> Shape {
        (self.0.len(), self.0[0].len()).into()
    }

    /// Flatten the wrapped [`Vec<Vec<f32>>`] into a ['Vec<f32>'] by
    /// concatenating the columns.
    fn as_flattened_vec(&self) -> FlattenedEmbeddings {
        let v = &self.0;
        v.iter()
            .flat_map(|e| e.to_owned())
            .collect::<Vec<f32>>()
            .into()
    }

    /// Convert this Embeddings into an Array2 suitable for transformation
    /// by the linfa DBSCAN algorithm.
    fn as_ndarray(&self) -> Result<Array2<f32>> {
        let shape = self.shape();

        Array2::from_shape_vec(shape.0, self.as_flattened_vec().into())
            .with_context(|| format!("Embeddings::as_ndarray, shape: {}", shape))
    }

    /// Debugging function: compute the distances between each pair
    /// of embeddings exhaustively. O(2^^n), so be careful.
    #[allow(dead_code)]
    fn compute_pairwise_distances(&self) -> Result<Array2<f32>> {
        use Array2;

        let embeddings = self
            .as_ndarray()
            .with_context(|| "Embeddings::compute_pairwise_distances()")?;

        let n = embeddings.nrows();
        let mut distances = Array2::zeros((n, n));
        for i in 0..n {
            for j in 0..n {
                let diff = &embeddings.row(i) - &embeddings.row(j);
                distances[[i, j]] = diff.dot(&diff).sqrt();
            }
        }
        Ok(distances)
    }
}

/// Creates a single-column [`Embeddings`] instance
/// from a [`Vec<f32>`].
impl From<Vec<f32>> for Embeddings {
    fn from(value: Vec<f32>) -> Self {
        Self(vec![value])
    }
}

impl From<Vec<Embeddings>> for Embeddings {
    fn from(value: Vec<Embeddings>) -> Self {
        Self(
            value
                .into_iter()
                .fold(Vec::<Vec<f32>>::default(), |mut acc, mut nxt| {
                    acc.append(&mut nxt.0);
                    acc
                }),
        )
    }
}

/// Trait for types from which we want to generate embeddings.
pub trait Normalizable {
    /// Method called by `normalize()` prior to lowercasing and removing
    /// non-alpha characters.
    fn pre_normalize(&self) -> &str;

    /// Lowercases and removes non-alpha characters from the string.
    fn normalize(&self) -> String {
        self.pre_normalize()
            .to_lowercase()
            .replace(|c: char| !c.is_alphabetic(), "")
    }
}

impl Normalizable for Tag {
    fn pre_normalize<'a>(&'a self) -> &'a str {
        let s: &str = self.as_ref();
        let splits = s.rsplitn(2, "::").collect::<Vec<&'a str>>();
        splits.first().unwrap()
    }
}

/// Container for a mapping of cluster IDs to Cluster Members.
#[derive(Debug, Clone, PartialEq, IntoIterator, From, Into, Default)]
struct Clusters(BTreeMap<ClusterId, Vec<usize>>);
impl Clusters {
    /// Associate a member of a feature tensor, identified by (row) index, to a cluster
    /// identified by the given [`ClusterId`]
    fn add_cluster_assignment(&mut self, member_idx: usize, cluster_id: ClusterId) {
        self.0.entry(cluster_id).or_default().push(member_idx)
    }
}

/// Simple newtype wrapper around a ClusterID.
#[derive(
    Debug,
    Clone,
    PartialEq,
    PartialOrd,
    Ord,
    Eq,
    Hash,
    Copy,
    From,
    Into,
    Default,
    AsRef,
    AsMut,
    Deref,
    DerefMut,
    Display,
    Serialize,
    Deserialize,
)]
pub struct ClusterId(usize);

impl ClusterId {
    /// The cluster id assigned to the "noise" cluster by DBSCAN
    pub const NOISE: Self = ClusterId(usize::MAX);

    /// If true... you guessed it... we're noise
    pub fn is_noise(&self) -> bool {
        *self == Self::NOISE
    }

    /// I can't believe it's not noise!
    pub fn is_signal(&self) -> bool {
        !self.is_noise()
    }
}

/// Provenance of the embeddings model used to generate the clusters.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
enum EmbeddingsProvenance {
    /// The provenance of the embeddings used to generate the clusters.
    /// This is a placeholder for now, but could be expanded in the future.
    SentenceEmbeddings(SentenceEmbeddingsModelType),
}

impl std::fmt::Display for EmbeddingsProvenance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EmbeddingsProvenance::SentenceEmbeddings(sentence_embeddings_model_type) => {
                write!(f, "{:?}", sentence_embeddings_model_type)
            }
        }
    }
}

/// Provenance of the embeddings model used to generate the clusters.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
enum ClusterAlgorithmProvenance {
    /// The provenance of the clustering algorithm used to generate the clusters.
    Dbscan(ClusterSize, DbscanEpsilon),
}

impl std::fmt::Display for ClusterAlgorithmProvenance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClusterAlgorithmProvenance::Dbscan(cluster_size, dbscan_epsilon) => {
                write!(
                    f,
                    "DBSCAN (cluster_size: {}, epsilon: {})",
                    cluster_size, dbscan_epsilon
                )
            }
        }
    }
}

/// Provenance of the clustering operation. Describes the algorithm
/// and embeddings model used to generate the clusters.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
struct ClusterProvenance {
    cluster_algorithm: ClusterAlgorithmProvenance,
    embeddings_model: EmbeddingsProvenance,
}

impl std::fmt::Display for ClusterProvenance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Model: {}; Clustering Algorithm: {}",
            self.embeddings_model, self.cluster_algorithm
        )
    }
}

/// The official result from a clustering operation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, IntoIterator)]
pub struct ClusteringProduct {
    provenance: ClusterProvenance,
    #[into_iterator]
    clusters: TagClusters,
}

impl ClusteringProduct {
    pub fn is_empty(&self) -> bool {
        self.clusters.is_empty()
    }

    /// True if we have at least one non-noise cluster.
    pub fn has_signal(&self) -> bool {
        self.clusters.has_signal()
    }

    /// Total number of clusters contained
    pub fn num_clusters(&self) -> usize {
        self.clusters.num_clusters()
    }

    /// Number of signal clusters contained
    pub fn num_signal_clusters(&self) -> usize {
        self.clusters.num_signal_clusters()
    }

    /// Number of tags contained
    pub fn num_tags(&self) -> usize {
        self.clusters.num_tags()
    }

    /// Number of non-noise tags contained
    pub fn num_signal_tags(&self) -> usize {
        self.clusters.num_signal_tags()
    }

    /// Proportion of signal tags to tag population.
    pub fn signal_proportion(&self) -> f32 {
        self.clusters.signal_proportion()
    }

    /// Proportion of noise tags to tag population
    pub fn noise_proportion(&self) -> f32 {
        1.0 - self.clusters.noise_proportion()
    }
}

impl std::fmt::Display for ClusteringProduct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Provenance:                 {}", self.provenance)?;
        writeln!(f, "Stats")?;
        writeln!(f, "- Cluster Count:              {}", self.num_clusters())?;
        writeln!(
            f,
            "- Cluster Count (Signal):     {}",
            self.num_signal_clusters()
        )?;
        writeln!(
            f,
            "- Cluster Count (Noise):      {}",
            self.num_clusters() - self.num_signal_clusters()
        )?;
        writeln!(f, "- Tag Count:                  {}", self.num_tags())?;
        writeln!(
            f,
            "- Tag Count (Signal):         {}",
            self.num_signal_tags()
        )?;
        writeln!(
            f,
            "- Tag Count (Noise):          {}",
            self.num_tags() - self.num_signal_tags()
        )?;
        writeln!(f, "------------------------------------")?;
        writeln!(f, "Clusters")?;
        writeln!(f, "{}", self.clusters)
    }
}

/// Represents a mapping of clusters to the [`Tag`] instances they contain.
#[derive(Debug, Clone, PartialEq, IntoIterator, From, Into, Default, Serialize, Deserialize)]
pub struct TagClusters(BTreeMap<ClusterId, Tags>);

impl From<(Tags, Clusters)> for TagClusters {
    fn from(value: (Tags, Clusters)) -> Self {
        Self::new(value.0, value.1)
    }
}

impl std::fmt::Display for TagClusters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (cluster_id, members) in self.clone().into_iter() {
            let cluster_name = match cluster_id {
                ClusterId(usize::MAX) => "Noise".to_string(),
                _ => format!("Cluster {}", cluster_id),
            };
            writeln!(f, "{}", cluster_name)?;
            for member in members.into_iter() {
                writeln!(f, "  - {}", member)?;
            }
            writeln!(f)?;
        }
        write!(f, "")
    }
}

impl TagClusters {
    fn new(tags: Tags, clusters: Clusters) -> Self {
        let mut ret = TagClusters::default();
        for (cluster_id, cluster_member_ids) in clusters.0 {
            for member_id in cluster_member_ids {
                ret.0
                    .entry(cluster_id)
                    .or_default()
                    .push(tags[member_id].clone());
            }
        }
        ret
    }

    /// Whether or not we're empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// True if we have at least one non-noise cluster.
    pub fn has_signal(&self) -> bool {
        self.num_signal_tags() > 0
    }

    /// Total number of clusters contained
    pub fn num_clusters(&self) -> usize {
        self.0.len()
    }

    /// Number of signal clusters contained
    pub fn num_signal_clusters(&self) -> usize {
        self.0.keys().filter(|k| !k.is_noise()).count()
    }

    /// Number of tags contained
    pub fn num_tags(&self) -> usize {
        self.0.values().fold(0, |acc, v| acc + v.len())
    }

    /// Number of non-noise tags contained
    pub fn num_signal_tags(&self) -> usize {
        self.0
            .keys()
            .filter(|k| !k.is_noise())
            .filter_map(|k| self.0.get(k))
            .count()
    }

    /// Proportion of signal tags to tag population.
    pub fn signal_proportion(&self) -> f32 {
        self.num_signal_tags() as f32 / self.num_tags() as f32
    }

    /// Proportion of noise tags to tag population
    pub fn noise_proportion(&self) -> f32 {
        1.0 - self.signal_proportion()
    }
}

/// Newtype for cluster sizes which enforces the invariant that they're >= 2.
#[derive(
    Debug, Copy, Clone, Hash, PartialEq, Eq, Into, AsRef, Deref, Serialize, Deserialize, Display,
)]
pub struct ClusterSize(usize);
impl ClusterSize {
    pub const MIN: ClusterSize = ClusterSize(2);
    pub const MAX: ClusterSize = ClusterSize(usize::MAX);

    /// Create a new instance given a `usize`, and panic if
    /// the following invariants are violated:
    /// - `size >= ClusterSize::MIN`
    /// - `size <= ClusterSize::MAX`
    fn new_unchecked(size: usize) -> Self {
        assert!(
            (Self::MIN.0..=Self::MAX.0).contains(&size),
            "Invalid value {}; must be {} ≤ N ≤ {}.",
            size,
            Self::MIN,
            Self::MAX
        );
        Self(size)
    }

    /// Attempt to create a new instance given a `usize`, and check
    /// invariants:
    /// - `size >= ClusterSize::MIN`
    /// - `size <= ClusterSize::MAX`
    pub fn try_new(size: usize) -> Result<Self> {
        if !(Self::MIN.0..=Self::MAX.0).contains(&size) {
            Err(anyhow!(
                "Invalid value {}; must be {} ≤ N ≤ {}.",
                size,
                Self::MIN,
                Self::MAX
            ))
        } else {
            Ok(Self::new_unchecked(size))
        }
    }
}

impl TryFrom<usize> for ClusterSize {
    type Error = anyhow::Error;

    fn try_from(value: usize) -> std::result::Result<Self, Self::Error> {
        Self::try_new(value)
    }
}

/// Newtype for epsilon (used for DBSCAN tolerance) which enforces the invariant that it's >= 0,0.
#[derive(
    Debug, Copy, Clone, PartialEq, PartialOrd, Into, AsRef, Deref, Serialize, Deserialize, Display,
)]
pub struct DbscanEpsilon(f32);
impl DbscanEpsilon {
    pub const MIN: DbscanEpsilon = DbscanEpsilon(0.0);

    /// Attempt to create a new value from the given `f32`.
    /// Panics if invariants (notably, epsilon >= 0.0) are violated.
    fn new_unchecked(value: f32) -> Self {
        assert!(
            value >= Self::MIN.0,
            "invalid value {}; must be >= {}",
            value,
            Self::MIN
        );
        Self(value)
    }
    /// Attempt to create a new value from the given `f32`, ensuring
    /// invariants (notably, epsilon >= 0.0) are observed.
    pub fn try_new(epsilon: f32) -> Result<Self> {
        match epsilon {
            _ if epsilon >= Self::MIN.0 => Ok(Self::new_unchecked(epsilon)),
            _ => Err(anyhow!(
                "invalid value {}; must be >= {}",
                epsilon,
                Self::MIN
            )),
        }
    }
}

impl TryFrom<f32> for DbscanEpsilon {
    type Error = anyhow::Error;

    fn try_from(value: f32) -> std::result::Result<Self, Self::Error> {
        Self::try_new(value)
    }
}

// =====================================================================
// =====================================================================
//
//  ____________
// < utilities! >
// ------------
//        \   ^__^
//         \  (oo)\_______
//            (__)\       )\/\
//                ||----w |
//                ||     ||
//

/// Native enum that maps to a [`SentenceEmbeddingsModelType`].
/// This mirrors [`SentenceEmbeddingModelType`], but can receive
/// derived / imoplemented traits, etc, which the foreign type cannot.
///
/// **NOTE:** This should be turned into a proc macro, as this won't be
/// the only t ime we need to do this for.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Serialize,
    Deserialize,
    clap::ValueEnum,
    Default,
    IntoStaticStr,
    EnumString,
    Display,
)]
pub enum SentenceEmbeddingsModelType {
    DistiluseBaseMultilingualCased,
    BertBaseNliMeanTokens,
    AllMiniLmL12V2,
    AllMiniLmL6V2,
    AllDistilrobertaV1,
    ParaphraseAlbertSmallV2,
    #[default]
    SentenceT5Base,
}

impl From<SentenceEmbeddingsModelType> for sentence_embeddings::SentenceEmbeddingsModelType {
    fn from(val: SentenceEmbeddingsModelType) -> Self {
        match val {
            SentenceEmbeddingsModelType::DistiluseBaseMultilingualCased => {
                sentence_embeddings::SentenceEmbeddingsModelType::DistiluseBaseMultilingualCased
            }
            SentenceEmbeddingsModelType::BertBaseNliMeanTokens => {
                sentence_embeddings::SentenceEmbeddingsModelType::BertBaseNliMeanTokens
            }
            SentenceEmbeddingsModelType::AllMiniLmL12V2 => {
                sentence_embeddings::SentenceEmbeddingsModelType::AllMiniLmL12V2
            }
            SentenceEmbeddingsModelType::AllMiniLmL6V2 => {
                sentence_embeddings::SentenceEmbeddingsModelType::AllMiniLmL6V2
            }
            SentenceEmbeddingsModelType::AllDistilrobertaV1 => {
                sentence_embeddings::SentenceEmbeddingsModelType::AllDistilrobertaV1
            }
            SentenceEmbeddingsModelType::ParaphraseAlbertSmallV2 => {
                sentence_embeddings::SentenceEmbeddingsModelType::ParaphraseAlbertSmallV2
            }
            SentenceEmbeddingsModelType::SentenceT5Base => {
                sentence_embeddings::SentenceEmbeddingsModelType::SentenceT5Base
            }
        }
    }
}

impl From<SentenceEmbeddingsModelType> for PathBuf {
    fn from(value: SentenceEmbeddingsModelType) -> Self {
        PathBuf::from(Into::<&'static str>::into(value))
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use tch::Device;

    use crate::etq::{
        embeddings::{DbscanEpsilon, Normalizable},
        model::{Tag, Tags},
    };

    use super::{ClusterSize, SentenceEmbeddingsModelType, embed_and_cluster_tags};

    #[test]
    fn test_normalize_tag() {
        let tag = Tag::from_str("fw::kw::foo").unwrap();
        assert_eq!(tag.normalize(), "foo");

        let tag = Tag::from_str("fw::foo").unwrap();
        assert_eq!(tag.normalize(), "foo");

        let tag = Tag::from_str("foo").unwrap();
        assert_eq!(tag.normalize(), "foo");
    }

    #[test]
    fn test_cluster_size_checked() {
        assert!(ClusterSize::try_new(2).is_ok());
        assert!(ClusterSize::try_new(1000).is_ok());
        assert!(ClusterSize::try_new(ClusterSize::MAX.into()).is_ok());
        assert!(ClusterSize::try_new(ClusterSize::MIN.into()).is_ok());
        assert!(ClusterSize::try_new(ClusterSize::MIN.0 - 1).is_err());
        assert!(ClusterSize::try_new(0).is_err());
    }

    #[test]
    fn test_cluster_size_unchecked() {
        assert_eq!(
            ClusterSize::new_unchecked(ClusterSize::MIN.0),
            ClusterSize::MIN
        );
        assert_eq!(
            ClusterSize::new_unchecked(ClusterSize::MAX.0),
            ClusterSize::MAX
        );

        let result =
            std::panic::catch_unwind(|| ClusterSize::new_unchecked(ClusterSize::MIN.0 - 1));
        assert!(result.is_err())
    }

    #[test]
    fn test_epsilon_checked() {
        assert!(DbscanEpsilon::try_new(1.0).is_ok());
        assert!(DbscanEpsilon::try_new(DbscanEpsilon::MIN.into()).is_ok());
        assert!(DbscanEpsilon::try_new(DbscanEpsilon::MIN.0 - 0.1).is_err());
    }

    #[test]
    fn test_epsilon_unchecked() {
        assert_eq!(
            DbscanEpsilon::new_unchecked(DbscanEpsilon::MIN.0),
            DbscanEpsilon::MIN
        );
        assert_eq!(
            DbscanEpsilon::new_unchecked(DbscanEpsilon::MIN.0 + 1.0).0,
            DbscanEpsilon::MIN.0 + 1.0
        );

        let result =
            std::panic::catch_unwind(|| DbscanEpsilon::new_unchecked(DbscanEpsilon::MIN.0 - 0.1));
        assert!(result.is_err());
    }

    #[test]
    fn test_clusters() {
        let tags = Tags::from(
            vec![
                Tag::from_str("fw::kw::monkey"),
                Tag::from_str("fw::kw::cooo"),
                Tag::from_str("fw::kw::cat"),
                Tag::from_str("fw::cat::dog"),
                Tag::from_str("fw::cat::lion"),
                Tag::from_str("fw::cat::banana"),
                Tag::from_str("fw::kw::seed"),
                Tag::from_str("fw::kw::plantain"),
                Tag::from_str("fw::kw::onion"),
                Tag::from_str("fw::cat::turnip"),
                Tag::from_str("fw::cat::wolf"),
                Tag::from_str("fw::cat::yorkie"),
                Tag::from_str("fw::kw::donkey"),
                Tag::from_str("fw::kw::clue"),
                Tag::from_str("fw::kw::kick"),
                Tag::from_str("fw::cat::roar"),
                Tag::from_str("fw::cat::jungle"),
                Tag::from_str("fw::cat::cub"),
                Tag::from_str("fw::kw::apple"),
                Tag::from_str("fw::kw::core"),
                Tag::from_str("fw::kw::potato"),
                Tag::from_str("fw::cat::skin"),
                Tag::from_str("fw::cat::howl"),
                Tag::from_str("fw::cat::bark"),
                Tag::from_str("fw::kw::garden"),
                Tag::from_str("fw::kw::flower"),
                Tag::from_str("fw::kw::water"),
                Tag::from_str("fw::cat::dirt"),
                Tag::from_str("fw::cat::farm"),
                Tag::from_str("fw::cat::shovel"),
                Tag::from_str("fw::kw::leaf"),
                Tag::from_str("fw::kw::tree"),
                Tag::from_str("fw::cat::nut"),
                Tag::from_str("fw::cat::bucket"),
                Tag::from_str("fw::cat::gold"),
                Tag::from_str("#fg::pat::exit-velocity"),
                Tag::from_str("#fg::pat::triadic-risk-drift"),
                Tag::from_str("#fg::pat::expertise-mask"),
                Tag::from_str("#fg::pat::consensus-theater"),
                Tag::from_str("#fg::pat::maslow-inversion"),
                Tag::from_str("#fg::pat::evap-chamber"),
                Tag::from_str("#fg::pat::gaslight-pivot"),
                Tag::from_str("#fg::pat::looping-ask"),
                Tag::from_str("#fg::pat::stain-carrier"),
                Tag::from_str("#fg::pat::boundary-relay-interception"),
                Tag::from_str("#fg::pat::benevolent-mirror"),
                Tag::from_str("#fg::pat::distorted-trust-loop"),
                Tag::from_str("#fg::pat::invisible-decider"),
                Tag::from_str("#fg::pat::blame-gravity"),
                Tag::from_str("#fg::pat::delegated-decay"),
                Tag::from_str("#fg::pat::diffused-accountability-hollow-control"),
                Tag::from_str("#fg::pat::reframing"),
                Tag::from_str("#fg::pat::one-way-scope-ratchet"),
                Tag::from_str("#fg::pat::tired-irony-loop"),
                Tag::from_str("#fg::pat::soft-cult-of-vision"),
                Tag::from_str("#fg::pat::demo-theater"),
                Tag::from_str("#fg::pat::selective-trust-leverage"),
                Tag::from_str("#fg::pat::cargo-cult-culture"),
                Tag::from_str("#fg::pat::weaponized-ambiguity"),
                Tag::from_str("#fg::lens::engagement-diagnostics"),
                Tag::from_str("#fg::sec::glossary"),
                Tag::from_str("#fg::strat::repititionresistance"),
                Tag::from_str("#fg::cat::behavioral-pattern"),
                Tag::from_str("#fg::cat::narrative-control"),
                Tag::from_str("#fg::cat::structural"),
                Tag::from_str("#fg::cat::navigational"),
                Tag::from_str("#fg::cat::organizational-dysfunction"),
                Tag::from_str("#fg::cat::behavioral"),
                Tag::from_str("#fg::cat::cultural-pattern"),
                Tag::from_str("#fg::env::accelerator-backed-org"),
                Tag::from_str("#fg::env::remote-org"),
                Tag::from_str("#fg::env::scaling-nonprofit"),
                Tag::from_str("#fg::env::flat-structure"),
                Tag::from_str("#fg::env::mission-driven-nonprofit"),
                Tag::from_str("#fg::env::consensus-driven"),
                Tag::from_str("#fg::env::founder-led-startup"),
                Tag::from_str("#fg::env::growth-stage-startup"),
                Tag::from_str("#fg::env::vision-centric-tech-org"),
                Tag::from_str("#fg::env::remote-collective"),
                Tag::from_str("#fg::tool::exitplan"),
                Tag::from_str("#fg::tool::audittrail"),
                Tag::from_str("#fg::kw::groupdynamics"),
                Tag::from_str("#fg::kw::scientific-posture"),
                Tag::from_str("#fg::kw::founder-dynamics"),
                Tag::from_str("#fg::kw::asymmetric-trust"),
                Tag::from_str("#fg::kw::boundarysetting"),
                Tag::from_str("#fg::kw::decision-latency"),
                Tag::from_str("#fg::kw::realitycheck"),
                Tag::from_str("#fg::kw::boundary-setting"),
                Tag::from_str("#fg::kw::scope-creep"),
                Tag::from_str("#fg::kw::evaporation"),
                Tag::from_str("#fg::kw::ethical-framing"),
                Tag::from_str("#fg::kw::trust-distortion"),
                Tag::from_str("#fg::kw::pretextual-framing"),
                Tag::from_str("#fg::kw::blame-diffusion"),
                Tag::from_str("#fg::kw::triangulation"),
                Tag::from_str("#fg::kw::expectation-calibration"),
                Tag::from_str("#fg::kw::burden-shifting"),
                Tag::from_str("#fg::kw::documentation"),
                Tag::from_str("#fg::kw::asymmetric-leverage"),
                Tag::from_str("#fg::kw::symbol-without-substance"),
                Tag::from_str("#fg::kw::boundaries"),
                Tag::from_str("#fg::kw::ritual-theater"),
                Tag::from_str("#fg::kw::false-feedback"),
                Tag::from_str("#fg::kw::network-drift"),
                Tag::from_str("#fg::kw::trust"),
                Tag::from_str("#fg::kw::hollow-intermediary"),
                Tag::from_str("#fg::kw::exit-strategy"),
                Tag::from_str("#fg::kw::riskmanagement"),
                Tag::from_str("#fg::kw::triage"),
                Tag::from_str("#fg::kw::manipulative-framing"),
                Tag::from_str("#fg::kw::risk-transfer"),
                Tag::from_str("#fg::kw::nonconvergent-pattern"),
                Tag::from_str("#fg::kw::values"),
                Tag::from_str("#fg::kw::namingthething"),
                Tag::from_str("#fg::kw::contractor-dynamics"),
                Tag::from_str("#fg::kw::reputational-asymmetry"),
                Tag::from_str("#fg::kw::de-escalation"),
                Tag::from_str("#fg::kw::relationship-inversion"),
                Tag::from_str("#fg::kw::loop-disconnect"),
                Tag::from_str("#fg::kw::false-consensus"),
                Tag::from_str("#fg::kw::pseudo-empowerment"),
                Tag::from_str("#fg::kw::contract-structure"),
                Tag::from_str("#fg::kw::reputational-risk"),
                Tag::from_str("#fg::kw::accountability-vacuum"),
                Tag::from_str("#fg::kw::patternrecognition"),
                Tag::from_str("#fg::kw::budget-constraints"),
                Tag::from_str("#fg::kw::free-work"),
                Tag::from_str("#fg::kw::accountability-drift"),
                Tag::from_str("#fg::kw::aiboundaries"),
                Tag::from_str("#fg::kw::strategic-drift"),
                Tag::from_str("#fg::kw::phantom-ownership"),
                Tag::from_str("#fg::kw::reputational-capital"),
            ]
            .iter()
            .map(|t| t.to_owned().unwrap())
            .collect::<Vec<Tag>>(),
        );

        let maybe_tag_clusters = embed_and_cluster_tags(
            tags.clone(),
            Device::Cpu,
            SentenceEmbeddingsModelType::AllMiniLmL12V2,
            ClusterSize::MIN,
            1.05.try_into().expect("Hey bub, this is an invalid ϵ."),
            // 0.968,
        );

        #[cfg(test)]
        println!("{}", maybe_tag_clusters.as_ref().unwrap());

        assert!(
            maybe_tag_clusters.is_ok(),
            "Narrator: tag_clusters was not, in fact, ok."
        );

        let tag_clusters = maybe_tag_clusters.unwrap();
        assert_eq!(tag_clusters.num_tags(), tags.len());
        assert!(tag_clusters.num_signal_tags() > 0);
        assert!(tag_clusters.noise_proportion() > 0.0);
        assert!(tag_clusters.signal_proportion() > 0.0);
        assert!(!tag_clusters.is_empty());

        // Check that none of the clusters is empty.
        assert_eq!(
            tag_clusters
                .into_iter()
                .filter(|(_, v)| v.is_empty())
                .count(),
            0
        )
    }
}
