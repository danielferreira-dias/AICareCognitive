# AI-Care FastAPI Application

This project contains a FastAPI application for predicting medical conditions (like Parkinson's and Alzheimer's) using pre-trained models. The application is containerized with Docker for easy deployment.

---

## Prerequisites

Make sure you have the following installed on your machine before proceeding:
- **Python 3.12 or higher** (optional, only if running locally outside of Docker)
- **Docker**

---

## Building and Running the Docker Container

### Step 1: Build the Docker Image
Run the following command in the project directory (where the `Dockerfile` exists) to build the Docker image:

```bash
docker build -t ai-care .
```

- `-t ai-care`: Tags the built Docker image with the name `ai-care`.
- `.`: Specifies the current directory as the build context.

---

### Step 2: Run the Docker Container
Once the image is built, you can run it as a container with:

```bash
docker run -d -p 8000:8000 ai-care
```

- `-d`: Runs the container in detached (background) mode.
- `-p 8000:8000`: Maps port 8000 on your host to port 8000 in the container.
- `ai-care`: The name of the Docker image.

After this step, the FastAPI application will be accessible at: [http://localhost:8000]()

---

## Endpoints

### 1. **Health Check**
- **URL**: `/health`
- **Method**: `GET`
- **Response**:
  ```json
  {
    "status": "Server is up and running!"
  }
  ```

### 2. **Prediction**
- **URL**: `/predict`
- **Method**: `POST`
- **Payload**: A JSON object containing the required fields for prediction.

    ```

---

## Stopping the Container

To stop the running container, follow these steps:

1. List running containers:
   ```bash
   docker ps
   ```

2. Find the container ID or name for `ai-care`.

3. Stop the container:
   ```bash
   docker stop <container_id_or_name>
   ```

---

## Cleanup

### Remove the Container
To remove the container after stopping it:
```bash
docker rm <container_id_or_name>
```

### Remove the Docker Image
To delete the Docker image if it is no longer needed:
```bash
docker rmi ai-care
```

---

## Additional Notes

- Make sure the `models` folder containing the required model files (e.g., `parkinson_model.joblib`, `alzheimer_model.joblib`) and all necessary application files exist in the same directory as the `Dockerfile`.
- If you make any changes to the code, rebuild the Docker image with:
  ```bash
  docker build -t ai-care .
  ```

---