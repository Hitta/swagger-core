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

package com.wordnik.swagger.core

import scala.util.control.Breaks._
import com.wordnik.swagger.core._
import com.wordnik.swagger.core.util._
import com.wordnik.swagger.core.ApiValues._
import com.wordnik.swagger.core.util.TypeUtil

import org.slf4j.LoggerFactory

import java.lang.reflect.{ Type, Field, Modifier, Method }
import java.lang.annotation.Annotation
import javax.xml.bind.annotation._

import scala.collection.JavaConversions._
import collection.mutable.ListBuffer

object ApiMethodType {
  val GET = "GET"
  val PUT = "PUT"
  val DELETE = "DELETE"
  val POST = "POST"
  val HEAD = "HEAD"
}

trait ApiSpecParserTrait extends BaseApiParser {
  private val LOGGER = LoggerFactory.getLogger(classOf[ApiSpecParserTrait])
  // don't understand?  Ask @greggcarrier
  def hostClass: Class[_]
  def documentation: Documentation
  def apiEndpoint: Api
  def apiVersion: String
  def swaggerVersion: String
  def basePath: String
  def resourcePath: String
  def getPath(method: Method): String
  def processParamAnnotations(docParam: DocumentationParameter, paramAnnotations: Array[Annotation], method: Method): Boolean

  val TRAIT = "trait"

  // implement this to detect HTTP method type
//  def parseHttpMethod(method: Method, apiOperation: ApiOperation): String = {
//    LOGGER.debug("YYYYYYYYYYYYYYYYYYYYYYYYYYYY HTTP YYYYYYYYYYYYYYYYY");
//    return "GET"
//  }
  def parseHttpMethod(method: Method, apiOperation: ApiOperation): String


  def parse(): Documentation = {
    if (apiEndpoint != null)
      hostClass.getMethods.foreach(method => parseMethod(method))
    documentation.apiVersion = apiVersion
    documentation.swaggerVersion = swaggerVersion
    documentation.basePath = basePath
    documentation.resourcePath = resourcePath
    documentation
  }

  def parseApiParam(docParam: DocumentationParameter, apiParam: ApiParam, method: Method) {
    LOGGER.debug("YYYYYYYYY Method:" + method + "++++++ Parameter: " + docParam.name)
    docParam.name = readString(apiParam.name, docParam.name)
    docParam.description = readString(apiParam.value)
    docParam.defaultValue = readString(apiParam.defaultValue)
    try {
      docParam.allowableValues = convertToAllowableValues(apiParam.allowableValues)
    } catch {
      case e: RuntimeException =>
        LOGGER.error("Allowable values annotation is wrong in method  " + method +
          "for parameter " + docParam.name)
        e.printStackTrace()
    }
    docParam.required = apiParam.required
    docParam.allowMultiple = apiParam.allowMultiple
    docParam.paramAccess = readString(apiParam.access)
  }

  def parseMethod(method: Method): Any = {
    val meth = method;

//    var apiOperation = meth.getAnnotation(classOf[ApiOperation])
//    LOGGER.debug("Y----Y----Y---Y apiOperation:" + apiOperation)
//    if (apiOperation == null) {
//      val methTmp = getPapaMethod(meth);
//      apiOperation = methTmp.getAnnotation(classOf[ApiOperation])
//      LOGGER.debug("Y----Y----Y---Y apiOperation2:" + apiOperation)
//    }

    var apiOperation = getApiOperationAnnotationInPapaMethod(meth)


    //    var types = method.getParameterTypes()
    //    types.foreach(typ => {LOGGER.debug("Y----Y----Y---Y type:" + typ)})

    val apiErrors = meth.getAnnotation(classOf[ApiErrors])
    val isDeprecated = meth.getAnnotation(classOf[Deprecated])

    LOGGER.debug("parsing method " + meth.getName)
    if (apiOperation != null && meth.getName != "getHelp") {
      // Read the Operation
      val docOperation = new DocumentationOperation

      // check if its deprecated
      if (isDeprecated != null) docOperation.deprecated = true

      if (apiOperation != null) {
        LOGGER.debug("YYYYYYYYYYYYYYYYYYYYYYYYYYYY TIME TO PARSE HTTP METHOD:"  );
        docOperation.httpMethod = parseHttpMethod(meth, apiOperation)
        LOGGER.debug("YYYYYYYYYYYYYYYYYYYYYYYYYYYYapiOperation.value:" + apiOperation.value);
        docOperation.summary = readString(apiOperation.value)
        LOGGER.debug("YYYYYYYYYYYYYYYYYYYYYYYYYYYYapiOperation.notes:" +apiOperation.notes);
        docOperation.notes = readString(apiOperation.notes)
        docOperation.setTags(toObjectList(apiOperation.tags))
        docOperation.nickname = meth.getName
        val apiResponseValue = readString(apiOperation.responseClass)
        LOGGER.debug("YYYYYYYYYYYYYYYYYYYYYYYYYYYYapiOperation.responseClass:" +apiOperation.responseClass);
        val isResponseMultiValue = apiOperation.multiValueResponse

        docOperation.setResponseTypeInternal(apiResponseValue)
        try {
          val cls = SwaggerContext.loadClass(apiResponseValue)
          val annotatedName = ApiPropertiesReader.readName(cls)
          docOperation.responseClass = if (isResponseMultiValue) "List[" + annotatedName + "]" else annotatedName
        } catch {
          case e: ClassNotFoundException => docOperation.responseClass = apiResponseValue
        }
      }

      // Read method annotations for implicit api params which are not declared as actual argments to the method
      // Essentially ApiParamImplicit annotations on method
      val methodAnnotations = meth.getAnnotations
      for (ma <- methodAnnotations) {
        ma match {
          case pSet: ApiParamsImplicit => {
            for (p <- pSet.value()) {
              val docParam = new DocumentationParameter
              docParam.paramType = TYPE_QUERY

              docParam.name = readString(p.name)
              docParam.description = readString(p.value)
              docParam.defaultValue = readString(p.defaultValue)
              try {
                docParam.allowableValues = convertToAllowableValues(p.allowableValues)
              } catch {
                case e: RuntimeException =>
                  LOGGER.error("Allowable values annotation is wrong in method  " + method +
                    "for parameter " + docParam.name)
                  e.printStackTrace()
              }
              docParam.required = p.required
              docParam.allowMultiple = p.allowMultiple
              docParam.paramAccess = readString(p.access)
              docParam.internalDescription = readString(p.internalDescription)
              docParam.dataType = readString(p.dataType)
              docParam.paramType = readString(p.paramType)
              docParam.paramType = if (docParam.paramType == null) TYPE_QUERY else docParam.paramType

              docOperation.addParameter(docParam)
            }
          }
          case _ => Unit
        }
      }



            LOGGER.debug("YYYYYYYYYYYY%%%%%%%%%%%%%%%% READ PARAMS IN meth: " + meth.getName())
      LOGGER.debug("YYYYYYYYYYYY%%%%%%%%%%%%%%%% meth.getClass(): " + meth.getDeclaringClass())
      // Read the params and add to Operation
      var paramAnnotationDoubleArray = meth.getParameterAnnotations
      paramAnnotationDoubleArray = getParamAnnotationInPapaMethod(meth)


      val paramTypes = meth.getParameterTypes
      val genericParamTypes = meth.getGenericParameterTypes
      var counter = 0
      var ignoreParam = false
      paramAnnotationDoubleArray.foreach(paramAnnotations => {
        val docParam = new DocumentationParameter
        docParam.required = true

        // determine value type
        try {
          val paramTypeClass = paramTypes(counter)
          val paramTypeName = ApiPropertiesReader.getDataType(genericParamTypes(counter), paramTypeClass);
          docParam.dataType = paramTypeName
          if (!paramTypeClass.isPrimitive && !paramTypeClass.getName().contains("java.lang")) {
            docParam.setValueTypeInternal(ApiPropertiesReader.getGenericTypeParam(genericParamTypes(counter), paramTypeClass))
          }
        } catch {
          case e: Exception => LOGGER.error("Unable to determine datatype for param " + counter + " in method " + method, e)
        }

        ignoreParam = processParamAnnotations(docParam, paramAnnotations, meth)

        if (paramAnnotations.length == 0) {
          ignoreParam = true
        }

        counter = counter + 1

        // Set the default paramType, if nothing is assigned
        docParam.paramType = readString(TYPE_BODY, docParam.paramType)
        if (!ignoreParam) docOperation.addParameter(docParam)
      })

      // Get Endpoint
      val docEndpoint = getEndPoint(documentation, getPath(meth))

      // Add Operation to Endpoint
      docEndpoint.addOperation(processOperation(meth, docOperation))
      LOGGER.debug("added operation " + docOperation + " from method " + meth.getName)

      // Read the Errors and add to Response
      if (apiErrors != null) {
        for (apiError <- apiErrors.value) {
          val docError = new DocumentationError
          docError.code = apiError.code
          docError.reason = readString(apiError.reason)
          docOperation.addErrorResponse(docError)
        }
      }
    } else LOGGER.debug("skipping method " + method.getName)
  }

  protected def processOperation(method: Method, o: DocumentationOperation) = o

  private def getEndPoint(documentation: Documentation, path: String): DocumentationEndPoint = {
    var ep: DocumentationEndPoint = null

    if (documentation.getApis != null)
      documentation.getApis.foreach(endpoint => if (endpoint.path == path) ep = endpoint)

    if (ep == null) {
      val o = new DocumentationEndPoint(path, apiEndpoint.description)
      documentation.addApi(o)
      o
    } else ep
  }

  private def getCategory(method: Method): String = {
    val declaringInterface = ReflectionUtil.getDeclaringInterface(method)
    if (declaringInterface == null)
      null
    else {
      val simpleName = declaringInterface.getSimpleName
      if (simpleName.toLowerCase.endsWith(TRAIT) && simpleName.length > TRAIT.length)
        simpleName.substring(0, simpleName.length - TRAIT.length)
      else
        simpleName
    }
  }






  private def getPapaMethod(method: Method): Method = {
    var meth = method
         LOGGER.debug("Y----Y----Y---Y METHOD:" + method.getName())
    LOGGER.debug("Y----Y----Y---Y METHOD.getDeclaringClass:" + method.getDeclaringClass())

    LOGGER.debug("Y>>>>>>>>>>>>>>>>>> " + method.getDeclaringClass().getCanonicalName())

    if (method.getDeclaringClass().getSuperclass() != null && method.getDeclaringClass().getCanonicalName().contains("EnhancerByGuice")) {
      LOGGER.debug("Y>>>>>>>>>>>>>>>>>> 1")
      //      // FIXME
      //      var parameters: Array[Class[_]] = method.getParameterTypes()
      //      parameters.foreach(param => {LOGGER.debug("Y----Y----Y---Y type:" + param)})
      //
      //      apiOperation = method.getDeclaringClass().getSuperclass().getMethod(method.getName(), parameters.asInstanceOf[Class]).getAnnotation(classOf[ApiOperation])
      LOGGER.debug("Y>>>>>>>>>>>>>>>>>> superclass: " + method.getDeclaringClass().getSuperclass())
      var methods = method.getDeclaringClass().getSuperclass().getMethods()
      var i = 0
      var j = -1
      LOGGER.debug("Y>>>>>>>>>>>>>>>>>> 2")
      breakable {
        LOGGER.debug("Y>>>>>>>>>>>>>>>>>> 3")
        for (i <- 0 until (methods.length - 1)) {
          LOGGER.debug("Y>>>>>>>>>>>>>>>>>> 4: " + i)
          if (methods(i).getName().equals(method.getName())) {
            j = i
            LOGGER.debug("Y>>>>>>>>>>>>>>>>>> FOUND:" + j)
            break
          }
        }
      }
      if (j >= 0) {
        LOGGER.debug("Y>>>>>>>>>>>>>>>>>> YES:" + j)
        meth = methods(j)
      }
    }

    LOGGER.debug("Y----Y----Y---Y METHOD2:" + meth.getName())
    LOGGER.debug("Y----Y----Y---Y METHOD2.getDeclaringClass:" + meth.getDeclaringClass())

    return meth;
  }

  private def getApiOperationAnnotationInPapaMethod(method: Method): ApiOperation = {
    var apiOperation = method.getAnnotation(classOf[ApiOperation])
    LOGGER.debug("Y----Y----Y---Y apiOperation:" + apiOperation)
    if (apiOperation == null) {
      val methTmp = getPapaMethod(method);
      apiOperation = methTmp.getAnnotation(classOf[ApiOperation])
      LOGGER.debug("Y----Y----Y---Y apiOperation2:" + apiOperation)
    }
    return apiOperation
  }


  private def getParamAnnotationInPapaMethod(method: Method) :Array[Array[Annotation]] = {

      val methTmp = getPapaMethod(method);
      val params = methTmp.getParameterAnnotations

    return params
  }



}
