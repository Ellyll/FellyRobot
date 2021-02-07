FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /source

# copy fsproj and restore as distinct layers
COPY *.fsproj .
RUN dotnet restore

# copy and publish app and libraries
COPY . .
RUN dotnet restore && dotnet publish -c release -o /app --no-restore

# final stage/image
FROM mcr.microsoft.com/dotnet/runtime:5.0
WORKDIR /app
COPY --from=build /app .
ENTRYPOINT ["./FellyRobot"]