/* globals getJquerySafeId */

var LocationsFunctions = window.LocationsFunctions;

// undefined check is performed to avoid overwriting the state / functionality of the object
if (typeof (LocationsFunctions) === 'undefined') {
	LocationsFunctions = {
		isModalAvailableAndForOpening: function(modalID) {
			return $('#' + getJquerySafeId(modalID)).length !== 0 && $('#' + getJquerySafeId(modalID)).data('open') == '1';
		},

		openModal: function(modalID) {
			setTimeout(function() {$('#' + getJquerySafeId(modalID)).modal('show');}, 200);
			setTimeout(function() {$('#' + getJquerySafeId(modalID)).data('open', '0');}, 500);
		},

		sourceURL: '',

		// function for opening the Manage Locations modal that links to the IBPWorkbench application and ensuring that the proper modal window is re-opened on close of this one
		openLocationsModal: function() {
			$('#manageLocationModal').modal({backdrop: 'static', keyboard: true});

			LocationsFunctions.retrieveCurrentProjectID().done(function(projectID) {
				if (LocationsFunctions.sourceURL === '') {
					LocationsFunctions.retrieveProgramLocationURL().done(function(data) {
						LocationsFunctions.sourceURL = data;
						$('#locationFrame').attr('src', LocationsFunctions.sourceURL + projectID);
					});
				} else {
					$('#locationFrame').attr('src', LocationsFunctions.sourceURL + projectID);
				}
			});

		},

		// function that prepares and initializes both the Select2 dropdown containing the location list as well as the checkbox that toggles between displaying only favorite
		// locations or no. locationConversionFunction is provided as a parameter in case developers wish to change the construction of each select2 item, tho built-in function
		// will be used by default if this is not provided
		processLocationDropdownAndFavoritesCheckbox: function(locationSelectID, favoritesCheckboxID, allRadioButtonId, breedingLocationOnlyRadio, locationType, locationConversionFunction, locationIdValue) {
			LocationsFunctions.retrieveLocations().done(function(data) {
				if (!locationConversionFunction) {
					locationConversionFunction = LocationsFunctions.convertLocationToSelectItem;
				}

				var possibleValues;
				if (!locationType || locationType === 'breedingLocation') {
					possibleValues = LocationsFunctions.convertLocationsToSelectItemList(data.allBreedingLocations, locationConversionFunction);
				} else if (locationType === 'seedStorageLocation') {
					possibleValues = LocationsFunctions.convertLocationsToSelectItemList(data.allSeedStorageLocations, locationConversionFunction);
				}

				var isBreeding, isFavorite = false;
				if (locationIdValue) {
					var locationIdEnviroment = parseInt(locationIdValue);
					isBreeding = data.allBreedingLocations && data.allBreedingLocations.some(location => location.locid == locationIdEnviroment);
					if (isBreeding) {
						$('#' + breedingLocationOnlyRadio).prop("checked", true);
						isFavorite = data.allBreedingFavoritesLocations && data.allBreedingFavoritesLocations.some(location => location.locid == locationIdEnviroment);
					} else {
						$('#' + allRadioButtonId).prop("checked", true);
						isFavorite = data.favoriteLocations && data.favoriteLocations.some(location => location.locid == locationIdEnviroment);
					}
				} else {
					isFavorite = data && data.allBreedingFavoritesLocations && data.allBreedingFavoritesLocations.length > 0;
				}

				$('#' + favoritesCheckboxID).prop('checked', isFavorite);
				var allPossibleValues = LocationsFunctions.convertLocationsToSelectItemList(data.allLocations, locationConversionFunction);
				var favoritePossibleValues = LocationsFunctions.convertLocationsToSelectItemList(data.favoriteLocations, locationConversionFunction);
				var favoriteBreedingPossibleValues = LocationsFunctions.convertLocationsToSelectItemList(data.allBreedingFavoritesLocations, locationConversionFunction);

				var $favFilter = $('#' + getJquerySafeId(favoritesCheckboxID));
				var $allFilter = $('#' + getJquerySafeId(allRadioButtonId));
				var $breedingFilter = $('#' + getJquerySafeId(breedingLocationOnlyRadio));
				var $filters = $favFilter.add($allFilter).add($breedingFilter);

				var applyFilter = function () {
					var containsLocationSelected = false;
					var lastSelectedLocation = $('#' + getJquerySafeId(locationSelectID)).select2('val');
					if ($favFilter.is(':checked')) {
						if ($breedingFilter.is(':checked')) {
							containsLocationSelected = LocationsFunctions.existsLocationInDropdown(lastSelectedLocation, favoriteBreedingPossibleValues);
							LocationsFunctions.createSelect2Dropdown(favoriteBreedingPossibleValues, locationSelectID);
						} else {
							containsLocationSelected = LocationsFunctions.existsLocationInDropdown(lastSelectedLocation, favoritePossibleValues);
							LocationsFunctions.createSelect2Dropdown(favoritePossibleValues, locationSelectID);
						}
					} else if ($allFilter.is(':checked')) {
						containsLocationSelected = LocationsFunctions.existsLocationInDropdown(lastSelectedLocation, allPossibleValues);
						LocationsFunctions.createSelect2Dropdown(allPossibleValues, locationSelectID);
					} else {
						containsLocationSelected = LocationsFunctions.existsLocationInDropdown(lastSelectedLocation, possibleValues);
						LocationsFunctions.createSelect2Dropdown(possibleValues, locationSelectID);
					}

					if (!containsLocationSelected) {
						$('#' + getJquerySafeId(locationSelectID)).select2('val', '');
					}
				};
				applyFilter();

				if (locationIdValue) {
					$('#' + getJquerySafeId(locationSelectID)).select2('val', locationIdValue);
				}

				$filters.on('change', function() {
					applyFilter();
					var currentSelected = $('#' + getJquerySafeId(locationSelectID)).select2('data');

					if (currentSelected && currentSelected.id) {
						$('#' + getJquerySafeId(locationSelectID)).select2('val', currentSelected.id);
					}

				});
				
				$(document).off('location-update');
				$(document).on(
					'location-update',
					function() {
						LocationsFunctions.processLocationDropdownAndFavoritesCheckbox(locationSelectID, favoritesCheckboxID,
							allRadioButtonId, breedingLocationOnlyRadio, locationType, locationConversionFunction, locationIdValue);
					    //Recreate the location combo to make sure that the changes made are reflected in the UI
						recreateLocationCombo();
				});
			});
		},

		// FIXME: change declaration so function is not accessible to the outside
		createSelect2Dropdown: function(valuesData, targetID) {
			if (! valuesData) {
				valuesData = [];
			}
			var minResults = (valuesData.length > 0) ? 20: -1;

			$('#' + getJquerySafeId(targetID)).select2(
				{
					minimumResultsForSearch: minResults,
					initSelection: function(element, callback) {
						$.each(valuesData, function(index, value) {
							if (value.id == element.val()) {
								callback(value);
							}
						});
					},
					query: function(query) {
						var data = {
							results: valuesData
						};
						// return the array that matches
						data.results = $.grep(data.results, function(item) {
							return ($.fn.select2.defaults.matcher(query.term,
								item.text));

						});
						query.callback(data);
					}
				});
		},

		// FIXME: change declaration so function is not accessible to the outside
		convertLocationsToSelectItemList: function(locationList, locationConversionFunction) {

			var selectItemList = [];
			if (!locationList) {
				return selectItemList;
			}

			$.each(locationList, function(index, methodItem) {
				selectItemList.push(locationConversionFunction(methodItem));
			});

			return selectItemList;
		},

		// FIXME: change declaration so function is not accessible to the outside
		convertLocationToSelectItem: function(location) {
			var locNameDisplay = location.lname;
			if (location.labbr != null && location.labbr != '') {
				locNameDisplay  += ' - (' + location.labbr + ')';
			}
			return {
				id: location.locid,
				text: locNameDisplay,
				abbr: location.labbr,
				description: location.labbr
			};
		},

		retrieveLocations: function() {
			return $.ajax({
				url: '/Fieldbook/locations/getLocations',
				type: 'GET',
				cache: false,
				data: '',
				async: false,
				success: function(data) {
					if (data.success === '0') {
						showErrorMessage('page-message', data.errorMessage);
					}
				},
				error: function(jqXHR, textStatus, errorThrown) {
					console.log('The following error occurred: ' + textStatus, errorThrown);
				},
				complete: function() {
				}
			});
		},

		retrieveProgramLocationURL: function() {
			return $.get('/Fieldbook/locations/programLocationsURL');
		},

		retrieveCurrentProjectID: function() {
			return $.get('/Fieldbook/breedingMethod/programID');
		},

		existsLocationInDropdown(locationId, locationPossibleValues) {
			return locationId && locationPossibleValues && locationPossibleValues.some(location => location.id == parseInt(locationId));
		}

	};
}
