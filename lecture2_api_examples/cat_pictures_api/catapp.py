from fastapi import FastAPI, HTTPException
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles
import os
import random
from typing import Optional
app = FastAPI()

# Directory where images are stored
cat_pic_dir = "images"

# Make sure that directory exists (if not, make it)
if not os.path.exists(cat_pic_dir):
  os.makedirs(cat_pic_dir)

@app.get("/")
def read_root():
    return {"message": "Welcome to the Cat Pics API. Use /random to get a random picture of our cats."}
  
@app.get("/random")
def get_random_image(cat_name: Optional[str] = None):
  # List all image files in the directory
  image_files = [f for f in os.listdir(cat_pic_dir) if os.path.isfile(os.path.join(cat_pic_dir, f))]
  
  # Filter by cat name if specified
  if cat_name:
    cat_name = cat_name.lower()
    if cat_name not in ["gal", "oppie"]:
      raise HTTPException(status_code=400, detail="Cat name must be Gal or Oppie")
    
    image_files = [f for f in image_files if cat_name in f.lower()]
    
  # Check if there are any images; if not, give an error
  if not image_files:
    raise HTTPException(status_code=404, detail="No images found")
  
  # Select a random image
  random_image = random.choice(image_files)
  image_path = os.path.join(cat_pic_dir, random_image)
  
  html_content = f"""
    <html>
        <head>
            <title>Random Picture of {cat_name}</title>
        </head>
        <body>
            <h1>Random picture of {cat_name}</h1>
            <img src="{image_path}" alt="Random Image" style="max-width: 30%; height: auto;">
        </body>
    </html>
    """
    
  return HTMLResponse(content=html_content)

app.mount("/images", StaticFiles(directory=cat_pic_dir), name="images")

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="127.0.0.1", port=8000)

# source this file with python app.py in terminal,
# then go to http://localhost:8000/api/greet or api/test!
# documentation is automatically generated at http://localhost:8000/docs
