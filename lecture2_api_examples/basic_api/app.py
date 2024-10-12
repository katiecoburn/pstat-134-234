from fastapi import FastAPI
app = FastAPI()

@app.get("/greet")
def greet():
  return {"message": "Hello, friend!"}

if __name__=="__main__":
  import uvicorn
  uvicorn.run(app, host="127.0.0.1", port=8000)
