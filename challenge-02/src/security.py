from fastapi import Depends, HTTPException, Security
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from jose import jwt, JWTError
import requests

# Auth0 configuration
AUTH0_DOMAIN = "aicare.eu.auth0.com"
API_AUDIENCE = "aicare-api"
ALGORITHMS = ["RS256"]

security = HTTPBearer()


# Decode and validate the JWT, and extract the 'sub' field
def extract_sub_from_token(
        credentials: HTTPAuthorizationCredentials = Security(HTTPBearer())
):
    token = credentials.credentials  # Extract the Bearer token
    try:
        # Get the JWKS from Auth0
        jwks_url = f"https://{AUTH0_DOMAIN}/.well-known/jwks.json"
        jwks = requests.get(jwks_url).json()

        # Match the 'kid' in the token header with a key in the JWKS
        unverified_header = jwt.get_unverified_header(token)
        rsa_key = {}
        for key in jwks["keys"]:
            if key["kid"] == unverified_header["kid"]:
                rsa_key = {
                    "kty": key["kty"],
                    "kid": key["kid"],
                    "use": key["use"],
                    "n": key["n"],
                    "e": key["e"]
                }
                break

        if not rsa_key:
            raise HTTPException(
                status_code=401,
                detail="Invalid token header. Unable to find appropriate key."
            )

        # Decode and validate the JWT
        payload = jwt.decode(
            token,
            rsa_key,
            algorithms=ALGORITHMS,
            audience=API_AUDIENCE,
            issuer=f"https://{AUTH0_DOMAIN}/"
        )

        # Extract the 'sub' claim
        sub = payload.get("sub")
        if not sub:
            raise HTTPException(status_code=401, detail="Token does not contain 'sub' field.")

        return sub

    except JWTError as e:
        raise HTTPException(status_code=401, detail=f"Invalid token: {str(e)}")