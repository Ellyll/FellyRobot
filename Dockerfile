#FROM mcr.microsoft.com/dotnet/sdk:5.0 AS base
FROM mcr.microsoft.com/dotnet/sdk:5.0.102-ca-patch-buster-slim AS base
WORKDIR /app

COPY FellyRobot.fsx .
COPY Config.json .

RUN apt-get update && apt-get install -y curl

CMD dotnet fsi /app/FellyRobot.fsx
