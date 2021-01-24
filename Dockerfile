FROM mcr.microsoft.com/dotnet/sdk:5.0 AS base
WORKDIR /app

COPY FellyRobot.fsx .
COPY Config.json .

RUN dotnet fsi FellyRobot.fsx
