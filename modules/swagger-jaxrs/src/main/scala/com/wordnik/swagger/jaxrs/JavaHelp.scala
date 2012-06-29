/**
 *  Copyright 2012 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.jaxrs

import org.slf4j.LoggerFactory
import java.io.IOException
import java.lang.reflect.Constructor
import javax.servlet.ServletConfig
import javax.ws.rs.core._
import javax.ws.rs.core.Response.Status
import javax.ws.rs._
import org.codehaus.jackson.JsonGenerationException
import org.codehaus.jackson.map.JsonMappingException
import com.sun.jersey.api.core.ResourceConfig
import com.wordnik.swagger.core._

abstract class JavaHelp {
  private val logger = LoggerFactory.getLogger(classOf[JavaHelp])

  @GET def getHelp(@Context servConfig: ServletConfig, @Context resConfig: ResourceConfig, @Context headers: HttpHeaders, @Context uriInfo: UriInfo): Response = {
    logger.debug("YYYYYY!!!!!!!!!YYYYYYYYY getHelp()")
    var configReader: ConfigReader = ConfigReaderFactory.getConfigReader(servConfig)
    var apiVersion: String = configReader.getApiVersion
    var swaggerVersion: String = configReader.getSwaggerVersion
    var basePath: String = configReader.getBasePath
    var apiFilterClassName: String = configReader.getApiFilterClassName
    var filterOutTopLevelApi: Boolean = true

    logger.debug("YYYYYY!!!!!!!!!YYYYYYYYY this.getClass:" + this.getClass.getSuperclass())
    val currentApiEndPoint = getApiAnnotationRecursively(this.getClass())

    //var currentApiEndPoint: Api = this.getClass.getAnnotation(classOf[Api])
    if (currentApiEndPoint == null) {
      logger.debug("YYYYYY!!!!!!!!!YYYYYYYYY currentApiEndPoint  NULL")
      return Response.status(Status.NOT_FOUND).build
    } else {
      var apiPath: String = null
      if (filterOutTopLevelApi) {
        apiPath = currentApiEndPoint.value
      } else apiPath = null
      var apiListingPath: String = null
      if (filterOutTopLevelApi) {
        if (!("" == currentApiEndPoint.listingPath)) apiListingPath = currentApiEndPoint.listingPath
        else apiListingPath = currentApiEndPoint.value
      } else apiListingPath = null
      var listingClass: Class[_] = this.getClass
      if (!("" == currentApiEndPoint.listingClass)) {
        listingClass = SwaggerContext.loadClass(currentApiEndPoint.listingClass)
      }
      var helpApi: HelpApi = new HelpApi(apiFilterClassName)
      var docs: Documentation = helpApi.filterDocs(JaxrsApiReader.read(listingClass, apiVersion, swaggerVersion, basePath, apiPath), headers, uriInfo, apiListingPath, apiPath)

      if (headers != null) logger.debug("YYYYYY!!!!!!!!!YYYYYYYYY headers.accept= " + headers.getRequestHeader("accept"));
      val acceptHeader = headers.getRequestHeader("accept")
      var mediaType = MediaType.APPLICATION_JSON;
      if (acceptHeader.contains(MediaType.APPLICATION_XML)) mediaType = MediaType.APPLICATION_XML
      return Response.ok.header(HttpHeaders.CONTENT_TYPE, mediaType).entity(docs).build
    }
  }

  private def getApiAnnotationRecursively(clazz: Class[_]): Api = {
    if (clazz == null) return null;
    logger.debug("YYYYYYY SEARCH FOR ANNOTATION @Api in class: " + clazz.toString())
    var apiAnnotation: Api = clazz.getAnnotation(classOf[Api])
    if (apiAnnotation == null) {
      val clazzTemp: Class[_ >: _] = clazz.getSuperclass
      apiAnnotation = getApiAnnotationRecursively(clazzTemp)
    }
    return apiAnnotation
  }
}

