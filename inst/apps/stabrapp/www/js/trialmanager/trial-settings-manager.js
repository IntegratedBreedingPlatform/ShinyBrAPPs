window.TrialSettingsManager = (function() {
	'use strict';

	var variableSelectionGroups = {},
		TrialSettingsManager, properties, selectedVariables, _translations;

	/*
	 * Finds a property in a list by id.
	 *
	 * @param {string} propertyId the id of the property to look for
	 * @param {object[]} propertyList the list of properties to search
	 * @returns {number} the index of the found property, or -1 if it was not found
	 */
	function _findPropertyById(propertyId, propertyList) {

		var index = -1;

		// Use each instead of grep to prevent the need to continue to iterate over the array once we've found what we're looking for
		$.each(propertyList, function(i, propertyObj) {

			var found = false;

			if (propertyObj.propertyId === propertyId) {
				found = true;
				index = i;
			}
			return !found;
		});
		return index;
	}

	/*
	 * Removes an item by item from a given list.
	 *
	 * @param {string} id the id of the item to remove
	 * @param {object[]} list the list of objects to search
	 * @param {string} idSelector the key with which the id property should be accessed on each item of the list (e.g. `id` or `propertyId`)
	 * @returns {object} the object that was removed or null if the property was not found
	 */
	function _removeById(id, list, idSelector) {

		var property = null;

		// Use each instead of grep to prevent the need to continue to iterate over the array once we've found what we're looking for
		$.each(list, function(index, obj) {

			var found = false;

			if (obj[idSelector] === id) {
				found = true;
				property = list.splice(index, 1)[0];
			}
			return !found;
		});
		return property;
	}

	/*
	 * For a list of given exclusions, ensure that if any of the variables are in a specified list of selected variables, their exclusions
	 * are removed from the given list of properties.
	 *
	 * @param {object} exclusions an exclusion object. Each key in the object represents a variable id that, if found, should have a matched
	 * variable removed. Each object in the exclusion should have two properties, `variableId` - the variable to remove, and `propertyId` -
	 * the id of the property of the variable to be removed.
	 * @param {object} selectedVariables an object representing all currently selected variables. Each key represents a variable id that is
	 * selected, and it's value the name of that variable.
	 * @param {object[]} properties a list of properties from which to remove variables / properties as appropriate
	 * @returns {object[]} a list of properties that were removed
	 */
	function _performExclusions(exclusions, selectedVariables, properties) {

		var removedProperties = [],
			key,
			exclusion,
			id,
			index;

		for (key in exclusions) {
			if (exclusions.hasOwnProperty(key)) {

				id = parseInt(key, 10);
				exclusion = exclusions[id];

				// If any of the variables that need translating are selected, we must remove it's counterpart
				if (typeof selectedVariables[id] !== 'undefined') {
					index = _findPropertyById(exclusion.propertyId, properties);

					// Remove the variable if we found the property. Weird if we can't find the property, but not an issue since we just
					// wanted to make sure one of it's variables was excluded
					if (index !== -1) {
						_removeById(exclusion.variableId, properties[index].standardVariables, 'id');

						// If the property has no more variables, remove it too
						if (properties[index].standardVariables.length === 0) {
							removedProperties.push(properties.splice(index, 1)[0]);
						}
					}
				}
			}
		}
		return removedProperties;
	}

	/*
	 * Remove the specified variables from the given list.
	 *
	 * @param {object[]} variableList a list of variables, each with a variableId and propertyId property
	 * @param {object[]} properties a list of properties from which to remove variables / properties as appropriate.
	 * @returns {object[]} a list of properties that were removed
	 */
	function _removeVariables(variableList, properties) {

		var removedProperties = [],
			index;

		$.each(variableList, function(i, variable) {

			index = _findPropertyById(variable.propertyId, properties);

			// Remove the variable if we found the property. Weird if we can't find the property, but not an issue since we just
			// wanted to make sure one of it's variables was excluded
			if (index !== -1) {
				_removeById(variable.variableId, properties[index].standardVariables, 'id');

				// If the property has no more variables, remove it too
				if (properties[index].standardVariables.length === 0) {
					removedProperties.push(properties.splice(index, 1)[0]);
				}
			}
		});
		return removedProperties;
	}

	function _removeVariablesByVariableId(variableList, properties) {
		var removedProperties = [];

		$.each(variableList, function(i, variable) {
			$.each(properties, function(key) {
				_removeById(parseInt(variable.variableId), properties[key].standardVariables, 'id');

				// If the property has no more variables, remove it too
				if (properties[key].standardVariables.length === 0) {
					removedProperties.push(properties.splice(key, 1)[0]);
				}

			});
		});

		return removedProperties;
	}

	TrialSettingsManager = function(translations) {
		this._dynamicExclusion = {};
		// Look for any existing variables and instaniate our list of them

		var isInt = function(value) {
			return !isNaN(value) &&
				parseInt(Number(value)) == value &&
				!isNaN(parseInt(value, 10));
		};

		$.each(translations, function(key, value) {
			if (isInt(key)) {
				var groupKey = parseInt(key, 10);

				variableSelectionGroups[groupKey] = {
					label: value.label,
					placeholder: value.placeholder
				};
			}
		});

		this._translations = translations;

	};

	/* FIXME - this logic should be in the back end
	 *
	 * Filters a list of properties according to some hard coded rules (see comments for details).
	 *
	 * @param {object[]} properties the list of properties to filter
	 * @param {number} group properties and variables will be filtered to be specific to the group represented by this number
	 * @returns {object} an object with two properties, excluded and included - the list of properties that were removed and left
	 */

	TrialSettingsManager.prototype._filterProperties = function(properties, selectedVariables, group) {

		// Don't modify the original list
		var filteredProperties = JSON.parse(JSON.stringify(properties)),

			studyLevelBreedingMethodPropertyId = 2670,

			selectionExclusions = {
				// Don't allow user to select BM_CODE_VTE from the Breeding method property if BM_ID_VTE is present
				8262: {
					variableId: 8252,
					propertyId: 2670
				}
			},

			exclusions = [],
			removedProperty;

		// Currently we only have special handling for management details (1) and selection strategy (6)
		switch (group) {
			case 1802:
				// Remove variables and properties as necessary
				exclusions = exclusions.concat(_performExclusions(selectionExclusions, selectedVariables, filteredProperties));
				break;
            case 1807:
                // Remove variables and properties as necessary
                exclusions = exclusions.concat(_performExclusions(selectionExclusions, selectedVariables, filteredProperties));
                break;
			default:
				break;
		}

		if (this._dynamicExclusion[group]) {
			if (Object.keys(this._dynamicExclusion[group]).length) {
				exclusions = exclusions.concat(_removeVariablesByVariableId(this._dynamicExclusion[group], filteredProperties));
			}
		}

		return {
			exclusions: exclusions,
			inclusions: filteredProperties
		};
	};

	TrialSettingsManager.prototype._initialiseVariableSelectionDialog = function() {
		this._variableSelection = new window.BMS.NurseryManager.VariableSelection({
			uniqueVariableError: this._translations.uniqueVariableError,
			variableSelectedMessage: this._translations.variableSelectedMessage,
			invalidAliasError: this._translations.invalidAliasError
		});

		return this._variableSelection;
	};

	TrialSettingsManager.prototype._addDynamicFilter = function(_list, _group) {
		this._dynamicExclusion[_group] = _list;
	};

	TrialSettingsManager.prototype._updateSelectedVariableList = function(selectedVariableList) {
		var idNameCombinationList = {

			// Mark PI_NAME as selected if PI_ID is present
			8110: 8100,

			// Mark COOPERATOR as selected if COOPERATOR_ID is present
			8372: 8373,

			// Mark LOCATION_NAME as selected if LOCATION_ID is present
			8190: 8180
		};

		$.each(selectedVariableList, function(termID, nameAlias) {
			if (idNameCombinationList.hasOwnProperty(termID)) {

				selectedVariableList[idNameCombinationList[termID]] = nameAlias;

			}
		});

		return selectedVariableList;
	};

	TrialSettingsManager.prototype._clearCache = function() {
		$.each(variableSelectionGroups, function(key) {
			delete variableSelectionGroups[key].data;
		});
	};

	TrialSettingsManager.prototype._openVariableSelectionDialog = function(params) {
		var groupId = parseInt(params.variableType, 10),
			group = variableSelectionGroups[groupId],
			groupTranslations = {
				label: group.label,
				placeholderLabel: group.placeholder
			},
			thisInstance = this,
			modal = this._variableSelection,
			apiUrl = (params.apiUrl) ? params.apiUrl : '/Fieldbook/manageSettings/settings/properties?type=' + groupId;

		// Initialise a variable selection modal if we haven't done so before
		if (!modal) {
			modal = this._variableSelection = this._initialiseVariableSelectionDialog();
		}

		selectedVariables = params.retrieveSelectedVariableFunction();
		selectedVariables = thisInstance._updateSelectedVariableList(selectedVariables);
		if(params.retrieveMappedTraits) {
			selectedVariables = thisInstance._updateSelectedVariableList(params.retrieveMappedTraits());
		}
		// If we haven't loaded data for this group before, then load it
		if (!group.data) {
			$.getJSON(apiUrl, function(data) {
				group.data = data;
				properties = thisInstance._filterProperties(group.data, selectedVariables, groupId);

				// Initialise a new Variable Selection instance, passing through the properties, group type and groupTranslations
				// TODO get variable usage
				modal.show(groupId, groupTranslations, {
					propertyData: properties.inclusions,
					excludedProperties: properties.exclusions,
					variableUsageData: [],
					selectedVariables: selectedVariables,
					callback: params.callback,
                    onHideCallback: params.onHideCallback,
                    options: params.options,
				});
			});

			// TODO Error handling

		} else {
			// We've shown this before, and have the data. Just show the dialog.
			properties = this._filterProperties(group.data, selectedVariables, groupId);
			modal.show(groupId, groupTranslations, {
				propertyData: properties.inclusions,
				excludedProperties: properties.exclusions,
				variableUsageData: [],
				selectedVariables: selectedVariables,
				callback: params.callback,
                onHideCallback: params.onHideCallback,
                options: params.options,
			});
		}
	};

	return TrialSettingsManager;
}());
