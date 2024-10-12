from fastapi import FastAPI, HTTPException
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles
import os
import random
from typing import Optional

app = FastAPI()

cat_pic_dir = "images"
# You could add code here to check if that directory existed
# and make the directory if not

@app.get("/")
def read_root():
  return {"message": "Welcome to the Cat Pics API. Use /random to get a random cat picture."}

@app.get("/random")
def get_random_cat_pic(cat_name: Optional[str] = None):
  # list all image files in the directory
  image_files = [f for f in os.listdir(cat_pic_dir) if os.path.isfile(os.path.join(cat_pic_dir, f))]
  
  # filter by cat name
  if cat_name:
    cat_name = cat_name.lower()
    if cat_name not in ["gal", "oppie"]:
      raise HTTPException(status_code=404, detail="Cat not in database")
    
    image_files = [f for f in image_files if cat_name in f.lower()]
  
  random_image = random.choice(image_files)
  image_path = os.path.join(cat_pic_dir, random_image)
  
  html_content = f"""
    <html>
      <head>
        <title> Cat Picture </title>
      </head>
      <body>
        <h1>Random picture of {cat_name}:</h1>
        <img src="{image_path}" style="max-width: 30%; height: auto;">
      </body>
    </html>
  """
  
  return HTMLResponse(content=html_content)

app.mount("/images", StaticFiles(directory = cat_pic_dir), name="images")

if __name__=="__main__":
  import uvicorn
  uvicorn.run(app, host="127.0.0.1", port=8000)
