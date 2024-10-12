# Import necessary libraries
from flask import Flask, render_template, request  # Import Flask and related modules for web handling
import requests  # Import requests library to make HTTP requests

# Create an instance of the Flask class
app = Flask(__name__)

# Function to get current weather data from WeatherAPI
def get_current_weather(api_key, location):
    # Construct the API URL with the provided API key and location
    url = f"http://api.weatherapi.com/v1/current.json?key=62e7a62b37024174a57225738240910&q={location}&aqi=no"
    
    # Send a GET request to the API
    response = requests.get(url)
    
    # Check if the response was successful
    if response.status_code == 200:
        # Return the JSON data if the request was successful
        return response.json()
    else:
        # Return None if there was an error
        return None

# Define the main route for the web app
@app.route("/", methods=["GET", "POST"])
def index():
    # Initialize weather variable to store weather data
    weather = None
    # Replace with your actual WeatherAPI key
    api_key = '62e7a62b37024174a57225738240910'  

    # Check if the request method is POST (form submission)
    if request.method == "POST":
        # Get the location input from the form
        location = request.form.get("location")
        # Call the function to get weather data for the specified location
        weather = get_current_weather(api_key, location)

    # Render the index.html template, passing in the weather data
    return render_template("index.html", weather=weather)

# Run the app if this script is executed directly
if __name__ == "__main__":
    app.run(debug=True)  # Start the Flask app in debug mode

