use std::{
    collections::HashMap,
    ops::Range,
    path::PathBuf,
};

use anyhow::{Context, Result};
use ndarray::Array2;
use plotters::prelude::*;

/// Plot a histogram based upon the given distance array.
/// The axes in the array are indices of points in multidimensional space
/// (in our case, the embeddings for a specific tag). Each
/// cell in the array represents the distance between the points
/// associated with the (i,j) indices.
///
/// Adapted from [evcxr jupyter integration](https://plotters-rs.github.io/plotters-doc-data/evcxr-jupyter-integration.html)
/// and [plotters/examples/histogram.rs](https://github.com/plotters-rs/plotters/blob/master/plotters/examples/histogram.rs).
pub fn plot_distance_hist(
    distances: &Array2<f32>,
    output_path: PathBuf,
    width: u32,
    height: u32,
    caption: &str,
    distance_range_override: Option<Range<f32>>,
) -> Result<PathBuf> {
    let plot_dimensions = (width, height);

    // zoinks, this is fugly. And I know there's got to be an easier way to do it.
    // But but we have to filter out both duplicate pairs (as we don't care about
    // directionality of distance), and we need to elide the diagonal, because
    // (D(i, j): i == j) == 0, and these are meaningless points.
    let distances = distances
        .indexed_iter()
        .fold(
            HashMap::<(usize, usize), _>::default(),
            |mut acc, ((i, j), v)| {
                if i != j {
                    let k = (i.min(j), i.max(j));
                    acc.insert(k, *v);
                    if *v < 0.1 {
                        println!("*******   {:?}: {}", k, *v)
                    }
                }
                acc
            },
        )
        .values()
        .cloned()
        .collect::<Vec<f32>>();

    let distance_range = distance_range_override.unwrap_or_else(|| {
        // I know there must be a better, SIMD-friendly way of doing this, but for our
        // purposes, right now, it's fine.
        let (min_distance, max_distance) =
            &distances.iter().fold((f32::MAX, f32::MIN), |acc, nxt| {
                (acc.0.min(*nxt), acc.1.max(*nxt))
            });
        (*min_distance)..(*max_distance)
    });

    // Bracing in this block just to assure the BC that the borrow
    // of output_path is dropped before returning.
    {
        let caption_style = FontDesc::from(("Zen Maru Gothic Light", 40));
        let label_style = FontDesc::from(("Zen Maru Gothic Light", 30));
        let num_points = distances.len();
        let bar_margin = 10;
        let step = 0.02;
        let count_range = 0..(num_points / 8);

        println!(
            "distances_range: {:?} count_range: {:?}",
            distance_range, count_range
        );

        let root = BitMapBackend::new(&output_path, plot_dimensions).into_drawing_area();
        root.fill(&WHITE)
            .with_context(|| "distance_hist: fill background")?;

        let mut ctx = ChartBuilder::on(&root)
            .caption(
                format!("{}\n({} datapoints)", caption, num_points),
                caption_style,
            )
            .x_label_area_size(80)
            .y_label_area_size(80)
            .margin(bar_margin)
            .build_cartesian_2d(
                distance_range.step(step).use_round().into_segmented(),
                count_range,
            )
            .with_context(|| "distances_hist: build_cartesian_2d")?;

        ctx.configure_mesh()
            .disable_x_mesh()
            // .disable_y_mesh()
            .x_desc("Distance Bucket")
            .x_label_style(label_style.clone())
            .y_labels(5)
            .y_desc("Count")
            .y_label_style(label_style.clone())
            .y_label_formatter(&|y| format!("{}", *y))
            .draw()
            .with_context(|| "distances_hist(): draw mesh")?;

        ctx.draw_series(
            Histogram::vertical(&ctx)
                .style(RED.mix(0.5).filled())
                .margin(4)
                .data(distances.iter().map(|x| (*x, 1))),
        )
        .with_context(|| "distances_hist(): draw series")?;
    }

    Ok(output_path)
}
