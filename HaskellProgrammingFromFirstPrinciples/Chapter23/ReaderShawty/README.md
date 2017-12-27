## Origin
From: 
* 19.6 __An end-to-end example: URL shortener__
* https://github.com/bitemyapp/shawty-prime/blob/master/app/Main.hs

## Chapter 22: Exercise
> Remember the URL shortener? Instead of manually passing the database connection rConn from main to the app function that generates a Scotty app, use ReaderT to make the database connection available. We know you haven’t seen the transformer variant yet and we’ll explain them soon, but you should try to do the transformation mechanically. Research as necessary using a search engine. Use this version of the app: https://github.com/bitemyapp/shawty-prime/blob/master/app/Main.hs

## Running

### Run redis from within docker
```bash
$ docker run -d -p 6379:6379 redis:2.8
```

redis version 2.8 was used simply as that was the image I had downloaded for another project

### See also

* https://stackoverflow.com/questions/14178889/what-is-the-purpose-of-the-reader-monad
* https://stackoverflow.com/questions/22703289/scotty-connection-pool-as-monad-reader
* https://stackoverflow.com/questions/23070235/how-do-i-add-the-reader-monad-to-scottys-monad
* https://stackoverflow.com/questions/28361505/scotty-monad-transformer-for-per-handler-reader
* https://www.fpcomplete.com/blog/2017/06/readert-design-pattern


