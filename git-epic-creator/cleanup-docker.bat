@echo off
echo Cleaning up Docker Compose environment...

echo.
echo Stopping and removing containers...
docker-compose down

echo.
echo Removing named volumes...
docker-compose down -v

echo.
echo Removing orphaned containers...
docker-compose down --remove-orphans

echo.
echo Cleaning up unused Docker resources...
docker system prune -f

echo.
echo Docker cleanup completed!
echo All containers, volumes, and temporary storage have been removed.
pause
