use turbofish::App;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
  App::default().serve().await?;
  Ok(())
}
