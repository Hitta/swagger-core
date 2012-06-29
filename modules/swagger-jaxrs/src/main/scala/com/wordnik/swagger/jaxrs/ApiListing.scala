/**
 * Copyright 2012 Wordnik, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.wordnik.swagger.jaxrs

import com.wordnik.swagger.core._

import org.slf4j.LoggerFactory

import com.sun.jersey.api.core.ResourceConfig

import javax.ws.rs.{ Path, GET }
import javax.ws.rs.core.{ UriInfo, HttpHeaders, Context, Response }
import javax.servlet.ServletConfig

import scala.collection.JavaConversions._
import java.lang.annotation.Annotation

trait ApiListing {
  private val logger = LoggerFactory.getLogger(classOf[ApiListing])

  @GET
  @ApiOperation(value = "Returns list of all available api endpoints",
    responseClass = "DocumentationEndPoint", multiValueResponse = true)
  def getAllApis(@Context sc: ServletConfig,
    @Context rc: ResourceConfig,
    @Context headers: HttpHeaders,
    @Context uriInfo: UriInfo): Response = {

    val configReader = ConfigReaderFactory.getConfigReader(sc)
    val apiVersion = configReader.getApiVersion()
    val swaggerVersion = configReader.getSwaggerVersion()
    val basePath = configReader.getBasePath()
    val apiFilterClassName = configReader.getApiFilterClassName()
    var apiFilter: AuthorizationFilter = null
    if (null != apiFilterClassName) {
      try {
        logger.debug("loading filter class " + apiFilterClassName)
        apiFilter = SwaggerContext.loadClass(apiFilterClassName).newInstance.asInstanceOf[AuthorizationFilter]
      } catch {
        case e: ClassNotFoundException => logger.error("Unable to resolve apiFilter class " + apiFilterClassName);
        case e: ClassCastException => logger.error("Unable to cast to apiFilter class " + apiFilterClassName);
      }
    }

    val resources = rc.getRootResourceClasses
    val apiListingEndpoint = this.getClass.getAnnotation(classOf[Api])
    val resourceListingType = this.getClass.getAnnotation(classOf[javax.ws.rs.Produces]).value.toSet

    val allApiDoc = new Documentation
    resources.foreach(resource => {
      logger.debug("YYYYY----------------------------------")
      val wsPath = resource.getAnnotation(classOf[Api])
      logger.debug("processing resource path " + wsPath)
      if (wsPath != null) {
        logger.debug("YYYYY wsPath.value: " + wsPath.value)
        logger.debug("YYYYY wsPath.description(): " + wsPath.description())
      }
      if (null != wsPath && wsPath.value != JaxrsApiReader.LIST_RESOURCES_PATH) {
        val path = {
          if ("" != wsPath.listingPath) wsPath.listingPath
          else wsPath.value + JaxrsApiReader.FORMAT_STRING
        }
        logger.debug("YYYYY wsPath.value: " + wsPath.value)
        logger.debug("YYYYY path: " + path)
        logger.debug("adding api " + path + ", " + wsPath.description())

        val shouldAddDocumentation = {
          if (wsPath.listingPath != "" && wsPath.listingClass == "") {
            logger.debug("skipping documentation for " + wsPath.listingPath + ", " + wsPath.value)
            false
          } else {
            logger.debug("adding documentation for " + wsPath.listingPath + ", " + wsPath.listingClass)
            true
          }
        }

        val hasCompatibleMediaType = {
          // check accept type first
          val resourceMediaType = {
            if (headers.getRequestHeaders().contains("Content-type")) {
              logger.debug("using content-type headers")
              val objs = new scala.collection.mutable.ListBuffer[String]
              headers.getRequestHeaders()("Content-type").foreach(h =>
                h.split(",").foreach(str => objs += str.trim))
              objs.toSet
            } else if (headers.getRequestHeaders().contains("Accept")) {
              logger.debug("using accept headers")
              val objs = new scala.collection.mutable.ListBuffer[String]
              headers.getRequestHeaders()("Accept").foreach(h =>
                h.split(",").foreach(str => objs += str.trim))
              objs.toSet
            } else {
              logger.debug("using produces annotations")
              resource.getAnnotation(classOf[javax.ws.rs.Produces]).value.toSet
            }
          }

          // nothing found, check produces type
          var hasMatch = false

          logger.debug("YYYYYYY Resource = " + resource)
          val produceAnnotation = getProduceAnnotationRecursively(resource)

          //resource.getAnnotation(classOf[javax.ws.rs.Produces]).value.foreach(rt => {
          produceAnnotation.value.foreach(rt => {

            logger.debug("YYYYYYY rt " + rt)
            var mediaType: String = rt;
            if (rt.indexOf(';') >= 0) {
                mediaType = rt.substring(0, rt.indexOf(';'));
            }
            if (resourceListingType.contains(mediaType)) {
              logger.debug("matched " + mediaType)
              hasMatch = true
            } else logger.debug("no match on " + mediaType)
          })
          hasMatch
        }

        if (!hasCompatibleMediaType) logger.debug("no compatible media types")

        logger.debug("YYYYYYY shouldAddDocumentation =" + shouldAddDocumentation)
        logger.debug("YYYYYYY hasCompatibleMediaType =" + hasCompatibleMediaType)
        if (shouldAddDocumentation && hasCompatibleMediaType) {
          // need to use the actual path (wsPath.value), not the listing path (wsPath.path)
          val realPath = wsPath.value
          logger.debug(path + ", " + wsPath.value)
          var api = new DocumentationEndPoint(path, wsPath.description())
          logger.debug("YYYYYYY !isApiAdded(allApiDoc, api) =" + !isApiAdded(allApiDoc, api))
          if (!isApiAdded(allApiDoc, api)) {
            logger.debug("YYYYYYY !isApiAdded(allApiDoc, api) = TRUE")
            if (null != apiFilter) {
              logger.debug("YYYYYYY apiFilter = " + apiFilter.toString())
              apiFilter match {
                case apiAuthFilter: ApiAuthorizationFilter => {
                  logger.debug("YYYY case apiAuthFilter")
                  if (apiAuthFilter.authorizeResource(realPath, headers, uriInfo)) {
                    logger.debug("apiAuthFilter: adding api " + realPath)
                    allApiDoc.addApi(api)
                  }
                }
                case fineGrainedApiAuthFilter: FineGrainedApiAuthorizationFilter => {
                  logger.debug("YYYY case fineGrainedApiAuthFilter")
                  if (fineGrainedApiAuthFilter.authorizeResource(realPath, api, headers, uriInfo)) {
                    logger.debug("fineGrainedApiAuthFilter: adding api " + realPath)
                    allApiDoc.addApi(api)
                  }
                }
                case _ =>
              }
            } else {
              logger.debug("YYYYYYY apiFilter = NULL")
              allApiDoc.addApi(api)
            }
          }
        }
      } else logger.debug("no data for path " + wsPath)
    })

    allApiDoc.swaggerVersion = swaggerVersion
    allApiDoc.basePath = basePath
    allApiDoc.apiVersion = apiVersion

    Response.ok.entity(allApiDoc).build
  }

  private def isApiAdded(allApiDoc: Documentation, endpoint: DocumentationEndPoint): Boolean = {
    var isAdded: Boolean = false
    if (allApiDoc.getApis != null) {
      for (addedApi <- allApiDoc.getApis()) {
        if (endpoint.path.equals(addedApi.path)) isAdded = true
      }
    }
    isAdded
  }

  private def getProduceAnnotationRecursively(clazz: Class[_]): javax.ws.rs.Produces = {
    if (clazz == null) return null;
    logger.debug("YYYYYYY SEARCH FOR ANNOTATION @Produce in class: " + clazz.toString())
    var produceAnnotation: javax.ws.rs.Produces = clazz.getAnnotation(classOf[javax.ws.rs.Produces])
    if (produceAnnotation == null) {
      val clazzTemp: Class[_ >: _] = clazz.getSuperclass
      produceAnnotation = getProduceAnnotationRecursively(clazzTemp)
    }
    return produceAnnotation
  }
}
