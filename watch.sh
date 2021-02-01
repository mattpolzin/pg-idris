fswatch -E --exclude='(.+\.idr~)|(.+\.swp.*)' -ro ./src | xargs -n1 -I{} ./learn.sh
