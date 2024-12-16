# Base image with Python 3.12
                           FROM python:3.12-slim

                           # Set the working directory in the container
                           WORKDIR /app

                           # Copy only the files necessary for installing dependencies (to leverage Docker caching)
                           COPY requirements.txt .

                           # Install dependencies
                           RUN pip install --no-cache-dir -r requirements.txt

                           # Copy the entire project into the container
                           COPY . .

                           # Expose the port FastAPI will listen on
                           EXPOSE 8000

                           # Command to run the FastAPI application using Uvicorn
                           CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000", "--workers", "4"]