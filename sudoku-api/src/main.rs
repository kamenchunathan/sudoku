#![feature(iter_array_chunks)]
use actix_cors::Cors;
use actix_web::{
    http, post,
    web::{self, Json},
    App, HttpServer,
};
use serde::{Deserialize, Serialize};
use sudoku_solver::{Board, SolutionIter};

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    println!("Staring server on port 8000");
    HttpServer::new(|| {
        let cors = Cors::default()
            .allowed_origin("http://localhost:5173")
            .allowed_methods(vec!["GET", "POST"])
            .allowed_headers(vec![
                http::header::AUTHORIZATION,
                http::header::ACCEPT,
                http::header::CONTENT_TYPE,
            ]);
        App::new().wrap(cors).service(solve_sudoku)
    })
    .bind(("0.0.0.0", 8000))?
    .run()
    .await
}
#[derive(Deserialize)]
struct Puzzle {
    puzzle: String,
}

#[derive(Serialize)]
struct SolveResult {
    solvable: bool,
    puzzles: Vec<String>,
}

fn to_string(b: Board) -> String {
   let a =  b.to_string().replace(" ", "").replace("\n", "");
   println!("{:?}", a);
      a 
}
#[post("/solve")]
async fn solve_sudoku(data: web::Json<Puzzle>) -> Option<Json<SolveResult>> {
    let a: Vec<_> = data
        .puzzle
        .chars()
        .into_iter()
        .map(|v| {
            if v == '.' {
                0 as u8
            } else {
                v.to_digit(10).unwrap_or(0) as u8
            }
        })
        .array_chunks::<9>()
        .collect();

    let board = Board::from(&a.try_into().unwrap());
    let mut solutions = SolutionIter::new(&board).peekable();
    let first_solution = solutions.peek();

    Some(Json(SolveResult {
        solvable: first_solution.is_some(),
        puzzles: solutions.map(to_string).take(10).collect(),
    }))
}
