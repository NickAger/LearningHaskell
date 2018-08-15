From "the Haskell book":

## Rewriting Shawty
Remember the URL shortener? [Chapter 19] Instead of manually passing the database connection rConn from main to the app function that generates a Scotty app, use ReaderT to make the database connection available. We know you haven’t seen the transformer variant yet and we’ll explain them soon, but you should try to do the transformation mechanically. Research as necessary using a search engine. Use this version of the app: https: //github.com/bitemyapp/shawty- prime/blob/master/app/Main.hs
