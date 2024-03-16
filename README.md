# diffnews

These are the files used to update the daily R news changes at https://developer.r-project.org/RSSfeeds.html .

### Notes

- They are run daily at 12:40 AM using this crontab entry:
```
40 0 * * *       /usr/local/bin/Rscript.dev /home/murdoch/blosxom/diffNEWS.R >/home/murdoch/blosxom/diffNEWS.log 2>&1
```

- When a new R release branch is created, manual intervention is
needed.  That happens once a year.  The update process is
described in the `HOW_TO_UPDATE.txt` file.

