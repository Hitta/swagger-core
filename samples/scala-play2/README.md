# Swagger Playframework Sample App

## Overview
This is a scala project to build a stand-alone server which implements the Swagger spec.  You can find out 
more about both the spec and the framework at http://swagger.wordnik.com.  For more information 
about Wordnik's APIs, please visit http://developer.wordnik.com.  There is an online version of this
server at http://petstore.swagger.wordnik.com/api/resources.json

### To build from source
Please follow instructions to build the top-level [swagger-core project](https://github.com/wordnik/swagger-core)


### To run
You currently need to build and publish the [swagger-play2](https://github.com/wordnik/swagger-play2) module.

````
cd modules/swagger-play2

play publish-local
````

then you can build the sample app:

````
play run
````

The application will listen on port 9000 and respond to `http://localhost:9000/resources.json`

### Limitations
Note the following limitations (which will go away very, very soon):

<li>- `api_key` filtering is not implemented</li>
