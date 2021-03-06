package controllers;

import api.*;

import com.wordnik.swagger.core.*;

import javax.ws.rs.*;

import models.Pet;
import play.*;
import play.mvc.*;

import views.html.*;

@Api(value = "/pet", description = "Operations about pets")
public class PetApiController extends BaseApiController {
	static PetData petData = new PetData();

	@Path("/{petId}")
	@ApiOperation(value = "Find pet by ID", notes = "Returns a pet when ID < 10. "
			+ "ID > 10 or nonintegers will simulate API error conditions", responseClass = "com.wordnik.swagger.sample.model.Pet")
	@ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
			@ApiError(code = 404, reason = "Pet not found") })
	public static Result getPet(
			@ApiParam(value = "ID of pet that needs to be fetched", allowableValues = "range[1,5]", required = true) @PathParam("petId") String petId) {
		Pet pet = new Pet();
		pet.setName("dog");
		pet.setId(Long.parseLong(petId));
		return JsonResponse(pet);
	}

	@POST
	@ApiOperation(value = "Add a new pet to the store")
	@ApiErrors(value = { @ApiError(code = 405, reason = "Invalid input") })
	@ApiParamsImplicit({@ApiParamImplicit(
			value = "Pet object that needs to be added to the store",
			required = true, dataType = "Pet", paramType = "body")})
	public static Result addPet() {
		Object o = request().body().asJson();
		petData.addPet((Pet)o);
		return JsonResponse("SUCCESS");
	}


	@PUT
	@ApiOperation(value = "Update an existing pet")
	@ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
			@ApiError(code = 404, reason = "Pet not found"),
			@ApiError(code = 405, reason = "Validation exception") })
	@ApiParamsImplicit({@ApiParamImplicit(
			value = "Pet object that needs to be updated in the store",
			required = true, dataType = "Pet", paramType = "body")})
	public static Result updatePet() {
		Object o = request().body().asJson();
		petData.addPet((Pet)o);
		return JsonResponse("SUCCESS");
	}

	@GET
	@Path("/findByStatus")
	@ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma seperated strings", responseClass = "com.wordnik.swagger.sample.model.Pet", multiValueResponse = true)
	@ApiErrors(value = { @ApiError(code = 400, reason = "Invalid status value") })
	public static Result findPetsByStatus(
			@ApiParam(value = "Status values that need to be considered for filter", required = true, defaultValue = "available", allowableValues = "available,pending,sold", allowMultiple = true) @QueryParam("status") String status) {
		return JsonResponse(petData.findPetByStatus(status));
	}

	@GET
	@Path("/findByTags")
	@ApiOperation(value = "Finds Pets by tags", notes = "Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.", responseClass = "com.wordnik.swagger.sample.model.Pet", multiValueResponse = true)
	@ApiErrors(value = { @ApiError(code = 400, reason = "Invalid tag value") })
	@Deprecated
	public static Result findPetsByTags(
			@ApiParam(value = "Tags to filter by", required = true, allowMultiple = true) @QueryParam("tags") String tags) {
		return JsonResponse(petData.findPetByTags(tags));
	}
}