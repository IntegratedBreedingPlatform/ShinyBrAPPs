/**
 * @module measurements-datatable
 */
if (typeof (BMS) === 'undefined') {
	BMS = {};
}
if (typeof (BMS.Fieldbook) === 'undefined') {
	BMS.Fieldbook = {};
}

BMS.Fieldbook.checkPagination = function(parentDiv) {
	'use strict';
	$(parentDiv + ' .dataTables_length select').on('change', function() {
		if ($(parentDiv + ' .fbk-page-div ul.pagination li').length > 3) {
			$(parentDiv + ' .fbk-page-div').removeClass('fbk-hide');
		}else {
			$(parentDiv + ' .fbk-page-div').addClass('fbk-hide');
		}
	});
};

BMS.Fieldbook.MeasurementsTable = {
	getColumnOrdering: function(tableName, forceGet) {
		var orderedColumns = [];
		var hasOrderingChange = false;
		if ($('body').data('columnReordered') === '1') {
			hasOrderingChange = true;
		}
		if ($('#' + tableName).dataTable() !== null &&  $('#' + tableName).dataTable().fnSettings() !== null) {
			var cols = $('#' + tableName).dataTable().fnSettings().aoColumns;
			$(cols).each(function(index) {
				var prevIndex = $('#' + tableName).dataTable().fnSettings().aoColumns[index]._ColReorder_iOrigCol;
				if (index != prevIndex) {
					hasOrderingChange = true;
				}

				var termId = $($(cols[index].nTh)[0]).attr('data-term-id');
				if (termId !== undefined) {
					orderedColumns[orderedColumns.length] = termId;
				}
			});
		}
		if (forceGet || hasOrderingChange) {
			return orderedColumns;
		}
		//we return blank if there is no ordering change
		return [];
	},

	containsHeader: function (tableName, header) {
		if ($('#' + tableName).dataTable() !== null && $('#' + tableName).dataTable().fnSettings() !== null) {
			var cols = $('#' + tableName).dataTable().fnSettings().aoColumns;
			for (var i = 0; i<cols.length; i++) {
				if (cols[i].termId == header) {
					return true;
				}
			};
		}
		return false;
	}
};

BMS.Fieldbook.ReviewDetailsOutOfBoundsDataTable = (function($) {

	/**
	 * Creates a new ReviewDetailsOutOfBoundsDataTable.
	 *
	 * @constructor
	 * @alias module:fieldbook-datatable
	 * @param {string} tableIdentifier the id of the table container
	 * @param {string} ajaxUrl the URL from which to retrieve table data
	 */
	var dataTableConstructor = function ReviewDetailsOutOfBoundsDataTable(tableIdentifier, dataList) {
		'use strict';

		var columns = [],
			columnsDef = [],
			table;

		$(tableIdentifier + ' thead tr th').each(function() {

			if (($(this).data('term-id') === 'Check')) {
				columns.push({
					data: 'active',
					defaultContent: '',
					render: function (data, type, row) {
						return '<input data-row-index="' + row.MEASUREMENT_ROW_INDEX + '" type="checkbox" class="editor-active" data-binding>';
					},
					className: 'fbk-center'
				});
			} else if (($(this).data('term-id') === 'NewValue')) {
				columns.push({
					data: 'newValue',
					defaultContent: '',
					render: function (data, type, row) {
						return '<input data-row-index="' + row.MEASUREMENT_ROW_INDEX + '" type="text" class="form-control" data-binding />';
					}
				});
			} else {
				columns.push({
					data: $(this).html(),
					defaultContent: '',
					render: function (data, type, row) {
						if (data && Array.isArray(data)) {
							return EscapeHTML.escape(data[0] ? data[0] : '');
						} else {
							return EscapeHTML.escape(data ? data : '');
						}
					}
				});
			}

			if ($(this).data('term-data-type-id') == '1130' || $(this).data('term-data-type-id') == '1110') {
				columnsDef.push({
					targets: columns.length - 1,
					defaultContent: '',
					render: function(data, type, full, meta) {
						return EscapeHTML.escape((data[0] != null) ? data[0] :  '');
					}
				});
			}
		});

		if ($.fn.dataTable.isDataTable($(tableIdentifier))) {
			table = $(tableIdentifier).DataTable();
			table.clear();
			table.rows.add(dataList).draw();
		} else {
			table = $(tableIdentifier).DataTable({
				data: dataList,
				columns: columns,
				retrieve: true,
				scrollY: '400px',
				scrollX: '100%',
				scrollCollapse: true,
				columnDefs: columnsDef,
				lengthMenu: [[50, 75, 100, -1], [50, 75, 100, 'All']],
				bAutoWidth: true,
				iDisplayLength: 100,
				fnRowCallback: function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {

					// Assuming ID is in last column
					$(nRow).attr('id', aData.experimentId);
					$(nRow).data('row-index', this.fnGetPosition(nRow));

					$('td', nRow).attr('nowrap', 'nowrap');
					$('td', nRow).attr('nowrap', 'nowrap');

					return nRow;
				},
				fnInitComplete: function(oSettings, json) {
					$(tableIdentifier + '_wrapper .dataTables_length select').select2({minimumResultsForSearch: 10});
					oSettings.oInstance.fnAdjustColumnSizing();
					oSettings.oInstance.api().colResize.init(oSettings.oInit.colResize);
				},
				language: {
					search: '<span class="mdt-filtering-label">Search:</span>'
				},
				dom: 'R<<"mdt-header"rli<"mdt-filtering">r><t>p>',
				// Problem with reordering plugin and fixed column for column re-ordering
				colReorder: {
					fixedColumns: 1
				}
			});
		}

		$(tableIdentifier).dataTable().bind('sort', function() {
			$(tableIdentifier).dataTable().fnAdjustColumnSizing();
		});

	};

	return dataTableConstructor;

})(jQuery);

BMS.Fieldbook.PreviewCrossesDataTable = (function($) {

	/**
	 * Creates a new PreviewCrossesDataTable.
	 *
	 * @constructor
	 * @alias module:fieldbook-datatable
	 * @param {string} tableIdentifier the id of the table container
	 * @param {string} ajaxUrl the URL from which to retrieve table data
	 */
	var dataTableConstructor = function PreviewCrossesDataTable(tableIdentifier, dataList, tableHeaderList, isImport, checkExistingCrosses) {
		'use strict';

		var columns = [],

			/*Column defs for female plot,male plot,crossing date,notes, breeding method,male nursery name(hide if cross is created)
			From Datatable API, using negative index counts from the last index of the columns (n-1)
			https://datatables.net/reference/option/columnDefs*/
			columnsDef = [
				{
					targets: [0],
					visible: checkExistingCrosses
				},
				{
					targets: [3],
					visible: isImport
				},
				{
					targets: [10,11, 12, 13],
					visible: false
				}
			],
			table;

		$.each( tableHeaderList, function( index, value ){
			columns.push({
				data: value,
				defaultContent: '',
			});
		});

		$(tableIdentifier + ' thead tr th').each(function(index) {
			if ($(this).html() === 'DUPLICATE') {
				columnsDef.push({
					defaultContent: '',
					targets: index,
					createdCell: function(td, cellData, rowData, row, col) {
						transformDuplicateStringToColorCodedSpans(td);
					}
				});
			} else if ($(this).html() === 'FEMALE PARENT') {
				columnsDef.push({
					targets: index,
					width: '100px',
					render: function(data, type, row) {
						return '<a class="gid-link" href="javascript: void(0)" ' +
							'onclick="ImportCrosses.openGermplasmModal(&quot;' +
							row.FGID + '&quot;,&quot;' + row['FEMALE_PARENT'] + '&quot;)">' + row['FEMALE_PARENT'] + '</a>';
					}
				});

			} else if ($(this).html() === 'MALE PARENT') {
				columnsDef.push({
					targets: index,
					width: '100px',
					render: function(data, type, row) {
						// Do not render as link if male parent is unknown
						if (row.MGID[0] === 0) {
							return row['MALE PARENT'];
						}
						// Render bracket-enclosed, comma-separated links for male parents
						var size = row.MGID.length;
						var str = size > 1 ? '[':'';
						$.each(row.MGID, function( index, value ) {
							str += '<a class="gid-link" href="javascript: void(0)" ' +
								'onclick="ImportCrosses.openGermplasmModal(&quot;' +
								row.MGID[index] + '&quot;,&quot;' + row['MALE_PARENT'][index] + '&quot;)">' + row['MALE_PARENT'][index] + '</a>'
							if (index < (size-1)) {
								str += ", ";
							}
						});
						if (size > 1) {
							str = str + "]";
						}
						return str;
					}
				});
			} else if ($(this).html() === 'ALERTS') {
				columnsDef.push({
					targets: index,
					width: '100px',
					render: function(data, type, row) {
						if(row.ALERTS) {
							return '<a class="gid-link" href="javascript: void(0)" ' +
								'onclick="ImportCrosses.viewExistingCrosses(&quot;' +
								row.FGID + '&quot;,&quot;' + row.MGID + '&quot;,&quot;' + row.CROSS + '&quot;' +
								',&quot;' + row['METHOD NUMBER'] + '&quot;,&quot;' + row.GID + '&quot;)">View Existing Crosses</a>';
						} else {
							return '';
						}
					}
				});

			}
			//update header with the correct ontology name
			$(this).html(columns[index].data);
		});

		if ($.fn.dataTable.isDataTable($(tableIdentifier))) {
			table = $(tableIdentifier).DataTable();
			table.clear();
			table.rows.add(dataList).draw();
		} else {
			table = $(tableIdentifier).DataTable({
				data: dataList,
				columns: columns,
				retrieve: true,
				scrollY: '400px',
				scrollX: '100%',
				scrollCollapse: true,
				order: [[1, 'asc']],
				columnDefs: columnsDef,
				lengthMenu: [[50, 75, 100, -1], [50, 75, 100, 'All']],
				bAutoWidth: true,
				iDisplayLength: 100,

				fnRowCallback: function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {

					// Assuming ID is in last column
					$(nRow).attr('id', aData.experimentId);
					$(nRow).data('row-index', this.fnGetPosition(nRow));

					$('td', nRow).attr('nowrap', 'nowrap');
					$('td', nRow).attr('nowrap', 'nowrap');

					return nRow;
				},
				fnInitComplete: function(oSettings, json) {
					$(tableIdentifier + '_wrapper .dataTables_length select').select2({minimumResultsForSearch: 10});
					oSettings.oInstance.fnAdjustColumnSizing();
					oSettings.oInstance.api().colResize.init(oSettings.oInit.colResize);
				},
				language: {
					search: '<span class="mdt-filtering-label">Search:</span>'
				},
				dom: 'R<<"mdt-header"rli<"mdt-filtering">r><t>p>',
				// Problem with reordering plugin and fixed column for column re-ordering
				colReorder: {
					fixedColumns: 1
				}
			});
		}

		$(tableIdentifier).DataTable().column( 3 ).visible( isImport );
		$(tableIdentifier).DataTable().column( 0 ).visible( checkExistingCrosses);

		$(tableIdentifier).dataTable().bind('sort', function() {
			$(tableIdentifier).dataTable().fnAdjustColumnSizing();
		});

	};

	return dataTableConstructor;

})(jQuery);

BMS.Fieldbook.GermplasmListDataTable = (function($) {


	/**
	 * Creates a new GermplasmListDataTable.
	 *
	 * @constructor
	 * @alias module:fieldbook-datatable
	 * @param {string} tableIdentifier the id of the table container
	 * @param {string} parentDiv parentdiv of that contains the table
	 * @param {dataList} json representation of the data to be displayed
	 */
	var dataTableConstructor = function GermplasmListDataTable(tableIdentifier, parentDiv, dataList) {
		'use strict';

		var columns = [],
			columnsDef = [],
			germplasmDataTable;

		$(tableIdentifier + ' thead tr th').each(function() {
			columns.push({data: $(this).data('col-name')});
			if ($(this).data('col-name') == 'gid') {
				// For GID
				columnsDef.push({
					targets: columns.length - 1,
					data: $(this).html(),
					width: '100px',
					render: function(data, type, full, meta) {
						return '<a class="gid-link" href="javascript: void(0)" ' +
							'onclick="openGermplasmDetailsPopopWithGidAndDesig(&quot;' +
							full.gid + '&quot;,&quot;' + full.desig + '&quot;)">' + data + '</a>';
					}
				});
			} else if ($(this).data('col-name') == 'desig') {
				// For designation
				columnsDef.push({
					targets: columns.length - 1,
					data: $(this).html(),
					render: function(data, type, full, meta) {
						return '<a class="desig-link" href="javascript: void(0)" ' +
							'onclick="openGermplasmDetailsPopopWithGidAndDesig(&quot;' +
							full.gid + '&quot;,&quot;' + full.desig + '&quot;)">' + data + '</a>';
					}
				});
			}
		});

		this.germplasmDataTable = $(tableIdentifier).dataTable({
			data: dataList,
			columns: columns,
			columnDefs: columnsDef,
			scrollY: '500px',
			scrollX: '100%',
			scrollCollapse: true,
			dom: 'R<t><"fbk-page-div"p>',
			iDisplayLength: 100,
			fnDrawCallback: function(oSettings) {
				makeGermplasmListDraggable(true);
			},
			fnRowCallback: function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
				$(nRow).data('entry', aData.entry);
				$(nRow).data('gid', aData.gid);
				$(nRow).data('index', aData.position);

				$(nRow).addClass('draggable primaryRow');
				$('td', nRow).attr('nowrap', 'nowrap');
				return nRow;
			},
			fnInitComplete: function(oSettings, json) {

				var totalPages = oSettings._iDisplayLength === -1 ? 0 : Math.ceil(oSettings.fnRecordsDisplay() / oSettings._iDisplayLength);
				if (totalPages === 1) {
					$(parentDiv + ' .fbk-page-div').addClass('fbk-hide');
				}
				$(parentDiv).removeClass('fbk-hide-opacity');
				oSettings.oInstance.fnAdjustColumnSizing();
				oSettings.oInstance.api().colResize.init(oSettings.oInit.colResize);
				oSettings.oInstance.fnAdjustColumnSizing();
			}
		});

		GermplasmListDataTable.prototype.getDataTable = function() {
			return this.germplasmDataTable;
		};
	};

	return dataTableConstructor;

})(jQuery);

BMS.Fieldbook.TrialGermplasmListDataTable = (function($) {

	var dataTableConstructor = function TrialGermplasmListDataTable(tableIdentifier, parentDiv, dataList) {
		'use strict';

		var columns = [],
			columnsDef = [],
			defaultOrdering = [],
			table;
		$(tableIdentifier + ' thead tr th').each(function() {
			columns.push({data: $(this).data('col-name')});
			if ($(this).data('col-name') == '8230-key') {
				defaultOrdering = [columns.length - 1, 'asc'];
			} else if ($(this).data('col-name') == '8240-key') {
				// For GID
				columnsDef.push({
					targets: columns.length - 1,
					data: $(this).data('col-name'),
					render: function(data, type, full, meta) {
						return '<a class="gid-link" href="javascript: void(0)" ' +
							'onclick="openGermplasmDetailsPopopWithGidAndDesig(&quot;' +
							full.gid + '&quot;,&quot;' + full.desig + '&quot;)">' + data + '</a>';
					}
				});
			} else if ($(this).data('col-name') == '8250-key') {
				// For designation
				columnsDef.push({
					targets: columns.length - 1,
					data: $(this).data('col-name'),
					render: function(data, type, full, meta) {
						return '<a class="desig-link" href="javascript: void(0)" ' +
							'onclick="openGermplasmDetailsPopopWithGidAndDesig(&quot;' +
							full.gid + '&quot;,&quot;' + full.desig + '&quot;)">' + data + '</a>';
					}
				});
			}else if ($(this).data('col-name') == '8255-key') {
				// For check
				columnsDef.push({
					targets: columns.length - 1,
					data: $(this).data('col-name'),
					render: function(data, type, full, meta) {
						var fieldName = 'selectedCheck',
							count = 0,
							actualVal = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;',
							actualCode = '',
							domElem = '';
						for (count = 0 ; count < full.checkOptions.length ; count++) {
							if (full.checkOptions[count].id == full['8255-key']) {
								actualVal = full.checkOptions[count].description;
								actualCode = full.checkOptions[count].name;
								domElem = '<input class="check-hidden" type="hidden"  data-code="' + actualCode + '" value="' + full['8255-key'] + '" id="selectedCheck' + (meta.row) + '" name="' + fieldName + '">';
								break;
							}
						}
						if (domElem === '') {
							domElem = '<input data-index="' + meta.row + '" class="check-hidden" type="hidden"  data-code="' + actualCode + '" value="' + full['8255-key'] + '" id="selectedCheck' + (meta.row) + '" name="' + fieldName + '">';
						}

						return '<a data-index="' + meta.row + '" class="check-href edit-check' + meta.row + '" data-code="' + actualCode + '" href="javascript: showPopoverCheck(&quot;' + (meta.row) + '&quot;, &quot;.germplasm-list-items&quot;, &quot;edit-check' + meta.row + '&quot;)">' + actualVal + '</a>' + domElem;
					}
				});
			} else if ($(this).data('col-name') == 'entry-checkbox') {
				// Checkbox for Entry selection
				columnsDef.push({
					targets: columns.length - 1,
					data: $(this).html(),
					width: '40px',
					render: function(data, type, full, meta) {
						return '<span><input type="checkbox" name="entryId" value="' + full.entryId + '"></span>';
					}
				});
			}
		});

		if ($.fn.dataTable.isDataTable($(tableIdentifier))) {
			this.table = $(tableIdentifier).DataTable();
			this.table.clear();
			this.table.rows.add(dataList).draw();
		} else {
			this.table = $(tableIdentifier).dataTable({
				data: dataList,
				columns: columns,
				columnDefs: columnsDef,
				retrieve: true,
				scrollY: '500px',
				scrollX: '100%',
				scrollCollapse: true,
				order: defaultOrdering,
				// Problem with reordering plugin and fixed column for column re-ordering
				colReorder: {
					fixedColumns: 1
				},
				dom: 'R<t><"fbk-page-div"p>',
				iDisplayLength: 100,
				fnDrawCallback: function(oSettings) {

				},
				fnRowCallback: function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
					$(nRow).data('entry', aData.entry);
					$(nRow).data('gid', aData.gid);
					$(nRow).data('index', aData.position);

					$(nRow).addClass('primaryRow');
					$('td', nRow).attr('nowrap', 'nowrap');
					return nRow;
				},
				fnInitComplete: function(oSettings, json) {
					var totalPages = oSettings._iDisplayLength === -1 ? 0 : Math.ceil(oSettings.fnRecordsDisplay() / oSettings._iDisplayLength);
					if (totalPages === 1) {
						$(parentDiv + ' .fbk-page-div').addClass('fbk-hide');
					}
					$(parentDiv).removeClass('fbk-hide-opacity');
					oSettings.oInstance.fnAdjustColumnSizing();
					oSettings.oInstance.api().colResize.init(oSettings.oInit.colResize);
					oSettings.oInstance.fnAdjustColumnSizing();
				}
			});
		}

		TrialGermplasmListDataTable.prototype.getDataTable = function() {
			return this.table;
		};

		TrialGermplasmListDataTable.prototype.getDataTableColumnIndex = function(colName)
		{
			var colNames = this.table.fnSettings().aoColumns;
			for (var counter = 0 ; counter < colNames.length ; counter++) {
				if (colNames[counter].data === colName) {
					return colNames[counter].idx;
				}
			}
			return -1;
		};

		TrialGermplasmListDataTable.prototype.getDataTableColumn = function(colName) {
			var colNames = this.table.fnSettings().aoColumns;
			for (var counter = 0 ; counter < colNames.length ; counter++) {
				if (colNames[counter].data === colName) {
					return colNames[counter];
				}
			}
			return null;
		};

		$('.col-show-hide').html('');
		$('.col-show-hide').html($(parentDiv + ' .mdt-columns').clone().removeClass('fbk-hide'));

		$('.germplasm-dropdown-menu a').click(function(e) {
			e.stopPropagation();
			if ($(this).parent().hasClass('fbk-dropdown-select-fade')) {
				$(this).parent().removeClass('fbk-dropdown-select-fade');
				$(this).parent().addClass('fbk-dropdown-select-highlight');

			} else {
				$(this).parent().addClass('fbk-dropdown-select-fade');
				$(this).parent().removeClass('fbk-dropdown-select-highlight');
			}

			// hide germplasm column
			(function(colName) {
				var column = null;
				// Get the column API object
				if (germplasmDataTable != null) {
					column = germplasmDataTable.getDataTableColumn(colName);
					// Toggle the visibility
					if (column !== null) {
						germplasmDataTable.getDataTable().fnSetColumnVis(column.idx, !column.bVisible);
					}
				}
			})($(this).attr('data-column-name'));

		});
	};

	return dataTableConstructor;

})(jQuery);

BMS.Fieldbook.AdvancedGermplasmListDataTable = (function($) {


	/**
	 * Creates a new AdvancedGermplasmListDataTable. This Datatable is the summary table view of the Advanced Germplasm list
	 *
	 * @constructor
	 * @alias module:fieldbook-datatable
	 * @param {string} tableIdentifier the id of the table container
	 * @param {string} parentDiv parentdiv of that contains the table
	 * @param {dataList} json representation of the data to be displayed
	 */
	var dataTableConstructor = function AdvancedGermplasmListDataTable(tableIdentifier, parentDiv, dataList) {
		'use strict';

		var germplasmDataTable;
		var _columnDefs = [
			// Column defs for trialInstanceNumber and replicationNumber will be visible for all studies
			// From Datatable API, using negative index counts from the last index of the columns (n-1)
			{
				targets: [ -1, -2 ],
				visible: true
			},
			// column defs for the entry checkbox selection, fix width
			{
				targets: [0],
				width: '38px'
			}
		];

		if ($.fn.dataTable.isDataTable($(tableIdentifier))) {
			this.germplasmDataTable = $(tableIdentifier).DataTable();
			this.germplasmDataTable.clear();
			this.germplasmDataTable.rows.add(dataList).draw();
		} else {
			this.germplasmDataTable = $(tableIdentifier).dataTable({
				columnDefs: _columnDefs,
				autoWidth: false,
				retrieve: true,
				scrollY: '500px',
				scrollX: '100%',
				scrollCollapse: true,
				dom: 'R<t><"fbk-page-div"p>',
				iDisplayLength: 100,
				fnDrawCallback: function(oSettings) {
					makeGermplasmListDraggable(true);
				},

				fnRowCallback: function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
					$(nRow).data('entry', aData.entry);
					$(nRow).data('gid', aData.gid);
					$('td', nRow).attr('nowrap', 'nowrap');
					return nRow;
				},
				fnInitComplete: function(oSettings, json) {
					var totalPages = oSettings._iDisplayLength === -1 ? 0 : Math.ceil(oSettings.fnRecordsDisplay() / oSettings._iDisplayLength);
					if (totalPages === 1) {
						$(parentDiv + ' .fbk-page-div').addClass('fbk-hide');
					}
					$(parentDiv).removeClass('fbk-hide-opacity');
					oSettings.oInstance.fnAdjustColumnSizing();
					oSettings.oInstance.api().colResize.init(oSettings.oInit.colResize);
					oSettings.oInstance.fnAdjustColumnSizing();
				}
			});
		}

		AdvancedGermplasmListDataTable.prototype.getDataTable = function()
		{
			return this.germplasmDataTable;
		};
	};
	return dataTableConstructor;
})(jQuery);

BMS.Fieldbook.PreviewDesignMeasurementsDataTable = (function($) {

	/**
	 * Creates a new PreviewDesignMeasurementsDataTable.
	 *
	 * @constructor
	 * @alias module:fieldbook-datatable
	 * @param {string} tableIdentifier the id of the table container
	 * @param {string} ajaxUrl the URL from which to retrieve table data
	 */
	var dataTableConstructor = function PreviewDesignMeasurementsDataTable(tableIdentifier, dataList) {
		'use strict';

		var columns = [],
			columnsDef = [],
			table;

		$(tableIdentifier + ' thead tr th').each(function() {
			// The undefined data upsets the datatable library and it gives a warning message -
			// "DataTables warning: table id={id} - Requested unknown parameter '{parameter}' for row {row-index}, column{column-index}`"
			// See http://datatables.net/manual/tech-notes/4
			// we need to set the {{defaultContent}} option so that nulls and undefined values were shown as empty string
			columns.push({
				data: $(this).html(),
				defaultContent: ''
			});
			if ($(this).data('term-id') == '8240') {
				// For GID
				columnsDef.push({
					targets: columns.length - 1,
					data: $(this).html(),
					width: '100px',
					render: function(data, type, full, meta) {
						return '<a class="gid-link" href="javascript: void(0)" ' +
							'onclick="openGermplasmDetailsPopopWithGidAndDesig(&quot;' +
							full.GID + '&quot;,&quot;' + full.DESIGNATION + '&quot;)">' + EscapeHTML.escape(data) + '</a>';
					}
				});
			} else if ($(this).data('term-id') == '8250') {
				// For designation
				columnsDef.push({
					targets: columns.length - 1,
					data: $(this).html(),
					render: function(data, type, full, meta) {
						return '<a class="desig-link" href="javascript: void(0)" ' +
							'onclick="openGermplasmDetailsPopopWithGidAndDesig(&quot;' +
							full.GID + '&quot;,&quot;' + full.DESIGNATION + '&quot;)">' + EscapeHTML.escape(data) + '</a>';
					}
				});
			} else if ($(this).data('term-id') == '8255' ) {
				columnsDef.push({
					targets: columns.length - 1,
					render: function(data, type, full, meta) {
						if(!!$(full).attr('ENTRY_TYPE')){
							return full.ENTRY_TYPE[0];
						}
					}
				})
			} else {
				columnsDef.push({
					targets: columns.length - 1,
					render: function(data, type, full, meta) {
						if (data !== undefined) {
							if (Array.isArray(data)) {
								return EscapeHTML.escape((data[0] != null) ? data[0] :  '');
							} else {
								return EscapeHTML.escape(data);
							}
						}
					}
				});
			}

		});

		if ($.fn.dataTable.isDataTable($(tableIdentifier))) {
			table = $(tableIdentifier).DataTable();
			table.clear();
			table.rows.add(dataList).draw();
		} else {
			table = $(tableIdentifier).DataTable({
				data: dataList,
				columns: columns,
				retrieve: true,
				scrollY: '600px',
				scrollX: '100%',
				scrollCollapse: true,
				columnDefs: columnsDef,
				lengthMenu: [[50, 75, 100, -1], [50, 75, 100, 'All']],
				bAutoWidth: true,
				iDisplayLength: 100,
				dom: 'R<<"mdt-header"rli<"mdt-filtering">r><t>p>',
				// For column visibility
				colVis: {
					exclude: [0],
					restore: 'Restore',
					showAll: 'Show all'
				},
				// Problem with reordering plugin and fixed column for column re-ordering
				colReorder: {
					fixedColumns: 1
				},
				fnInitComplete: function(oSettings, json) {
					oSettings.oInstance.fnAdjustColumnSizing();
				}
			});
		}
	};
	return dataTableConstructor;

})(jQuery);

// hook the context menu to the datatable cells with invalid values
// jquery-ui.contextmenu lib is replaced by jquery.contextMenu2.0. beforeOpen does not seem to be needed.
$(function() {
	$.contextMenu({
		// define which elements trigger this menu
		selector: "#fbk-measurements-controller-div .dataTable td[class*='invalid-value']",
		// define the elements of the menu
		callback: function (key, opt) {
			var colvindex = $(opt.$trigger.parent()).data('row-index');
			var termId = $(opt.$trigger).data('term-id');
			if (termId == null) {
				termId = $(opt.$trigger).parents('td').data('term-id');
				colvindex = $(opt.$trigger).parents('tr').data('row-index');
				opt.$trigger = $(opt.$trigger).parents('td');
			}
			switch (key) {
				case 'accept':
					markCellAsAccepted(colvindex, termId, opt.$trigger);
					break;
				case 'missing':
					markCellAsMissing(colvindex, termId, 'missing', 1, opt.$trigger);
					break;
			}

		},
		items: {
			accept: {
				name: "Accept value as-is"
			},
			missing: {
				name: "Set value to missing"
			}
		}
	});
});

function transformDuplicateStringToColorCodedSpans(td) {

	var possibleDupeOrRecip = $(td).text();
	var displayOfDuplicateColumn = possibleDupeOrRecip;
	// Clearing html of td which does not contain color code for duplication string
	$(td).html('');
	var plotDupe = "";
	var pedigreeDupe = "";
	var plotRecip = "";
	var pedigreeRecip = "";

	// Bifurcate possibleDupeOrRecip text as per pipe character and distribute its text to its desired variable
	// ex. plotDupe variable will get Plot Dupe: 4, 7
	var indexOfPipe = possibleDupeOrRecip.indexOf(" | ");
	while(indexOfPipe > 0) {
		if(possibleDupeOrRecip.indexOf("Plot Dupe:") > -1) {
			plotDupe = possibleDupeOrRecip.substring(0, indexOfPipe).trim();
			possibleDupeOrRecip = possibleDupeOrRecip.replace(plotDupe + " | ", "");
		} else if(possibleDupeOrRecip.indexOf("Pedigree Dupe:") > -1) {
			pedigreeDupe = possibleDupeOrRecip.substring(0, indexOfPipe).trim();
			possibleDupeOrRecip = possibleDupeOrRecip.replace(pedigreeDupe + " | ", "");
		} else if(possibleDupeOrRecip.indexOf("Plot Recip:") > -1) {
			plotRecip = possibleDupeOrRecip.substring(0, indexOfPipe).trim();
			possibleDupeOrRecip = possibleDupeOrRecip.replace(plotRecip + " | ", "");
		} else if(possibleDupeOrRecip.indexOf("Pedigree Recip:") > -1) {
			pedigreeRecip = possibleDupeOrRecip.substring(0, indexOfPipe).trim();
			possibleDupeOrRecip = possibleDupeOrRecip.replace(pedigreeRecip + " | ", "");
		}
		indexOfPipe = possibleDupeOrRecip.indexOf(" | ");
	}

	// If our possibleDupeOrRecip string is Plot Dupe: 4, 7 | Pedigree Dupe: 2, 6 | Plot Recip: 9, 8
	// then plotDupe and pedigreeDupe variables will get their information but last plotRecip will not get
	// For getting information of last dupe/recip we write following condition
	if(possibleDupeOrRecip != "") {
		if(possibleDupeOrRecip.indexOf("Plot Dupe:") > -1) {
			plotDupe = possibleDupeOrRecip;
		} else if(possibleDupeOrRecip.indexOf("Pedigree Dupe:") > -1) {
			pedigreeDupe = possibleDupeOrRecip;
		} else if(possibleDupeOrRecip.indexOf("Plot Recip:") > -1) {
			plotRecip = possibleDupeOrRecip;
		} else if(possibleDupeOrRecip.indexOf("Pedigree Recip:") > -1) {
			pedigreeRecip = possibleDupeOrRecip;
		}
	}

	// Previously, only one dupe/recip information and its color was displayed
	// To overcome this situation, we added different spans to one td so that it will set different colors to different spans
	if (displayOfDuplicateColumn.indexOf('Plot Dupe') != -1) {
		$(td).append("<span class='plotDupe'>" + plotDupe + "</span> ");
	}
	if (displayOfDuplicateColumn.indexOf('Pedigree Dupe') != -1) {
		$(td).append("<span class='pedigreeDupe'>" + pedigreeDupe + "</span> ");
	}
	if (displayOfDuplicateColumn.indexOf('Plot Recip') != -1) {
		$(td).append("<span class='plotRecip'>" + plotRecip + "</span> ");
	}
	if (displayOfDuplicateColumn.indexOf('Pedigree Recip') != -1) {
		$(td).append("<span class='pedigreeRecip'>" + pedigreeRecip + "</span> ");
	}

}

BMS.Fieldbook.FinalSampleListDataTable = (function($) {
	/**
	 * Creates a new SampleListDataTable.
	 *
	 * @constructor
	 * @alias module:fieldbook-datatable
	 * @param {string} tableIdentifier the id of the table container
	 * @param {string} parentDiv parentdiv of that contains the table
	 * @param {dataList} json representation of the data to be displayed
	 */
	var dataTableConstructor = function FinalSampleListDataTable(tableIdentifier, parentDiv, dataList, tableAutoWidth) {
		'use strict';

		var columns = [],
			aoColumnsDef = [],
			table;

		$(tableIdentifier + ' thead tr th').each(function(index) {
			columns.push({data: $(this).data('col-name')});
			aoColumnsDef.push({bSortable: false, bVisible: $(this).data('col-visible')});
		});

		table = $(tableIdentifier).dataTable({
			autoWidth: tableAutoWidth,
			scrollY: '500px',
			scrollX: '100%',
			scrollCollapse: true,
			columns: columns,
			aoColumns: aoColumnsDef,
			lengthMenu: [[50, 75, 100, -1], [50, 75, 100, 'All']],
			dom: 'R<"mdt-header" rli<"mdt-columns"B>><t><"fbk-page-div"p>',
			iDisplayLength: 100,
			buttons: [
				{
					extend: 'colvis',
					className: 'fbk-buttons-no-border fbk-colvis-button',
					text:'<i class="glyphicon glyphicon-th dropdown-toggle fbk-show-hide-grid-column"></i>'
				}
			]
		});

		FinalSampleListDataTable.prototype.getDataTable = function()
		{
			return table;
		};
	};

	return dataTableConstructor;

})(jQuery);

BMS.Fieldbook.ExistingCrossesDataTable = (function($) {

	/**
	 * Creates a new ExistingCrossesDataTable.
	 *
	 * @constructor
	 * @alias module:fieldbook-datatable
	 * @param {string} tableIdentifier the id of the table container
	 * @param {string} ajaxUrl the URL from which to retrieve table data
	 */
	var dataTableConstructor = function ExistingCrossesDataTable(tableIdentifier, dataList, tableHeaderList) {
		'use strict';

		var columns = [],
			columnsDef = [],
			table;

		$.each( tableHeaderList, function( index, value ){
			columns.push({
				data: value,
				defaultContent: '',
			});
		});

		$(tableIdentifier + ' thead tr th').each(function(index) {
			if ($(this).html() === 'GID') {
				columnsDef.push({
					targets: index,
					width: '100px',
					render: function(data, type, row) {
						return '<a class="gid-link" href="javascript: void(0)" ' +
							'onclick="ImportCrosses.openGermplasmModalFromExistingCrossesView(&quot;' +
							row.GID + '&quot;,&quot;' + row.DESIGNATION + '&quot;)">' + row['GID'] + '</a>';
					}
				});

			}
		});


		if ($.fn.dataTable.isDataTable($(tableIdentifier))) {
			table = $(tableIdentifier).DataTable();
			table.clear();
			table.rows.add(dataList).draw();
		} else {
			table = $(tableIdentifier).DataTable({
				data: dataList,
				columns: columns,
				retrieve: true,
				scrollY: '400px',
				scrollX: '100%',
				scrollCollapse: true,
				columnDefs: columnsDef,
				lengthMenu: [[50, 75, 100, -1], [50, 75, 100, 'All']],
				bAutoWidth: true,
				iDisplayLength: 100,

				fnRowCallback: function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {

					// Assuming ID is in last column
					$(nRow).attr('id', aData.experimentId);
					$(nRow).data('row-index', this.fnGetPosition(nRow));

					$('td', nRow).attr('nowrap', 'nowrap');
					$('td', nRow).attr('nowrap', 'nowrap');

					return nRow;
				},
				fnInitComplete: function(oSettings, json) {
					$(tableIdentifier + '_wrapper .dataTables_length select').select2({minimumResultsForSearch: 10});
					oSettings.oInstance.fnAdjustColumnSizing();
					oSettings.oInstance.api().colResize.init(oSettings.oInit.colResize);
				},
				language: {
					search: '<span class="mdt-filtering-label">Search:</span>'
				},
				dom: 'R<<"mdt-header"rli<"mdt-filtering">r><t>p>',
				// Problem with reordering plugin and fixed column for column re-ordering
				colReorder: {
					fixedColumns: 1
				}
			});
		}

		$(tableIdentifier).dataTable().bind('sort', function() {
			$(tableIdentifier).dataTable().fnAdjustColumnSizing();
		});

	};

	return dataTableConstructor;

})(jQuery);

