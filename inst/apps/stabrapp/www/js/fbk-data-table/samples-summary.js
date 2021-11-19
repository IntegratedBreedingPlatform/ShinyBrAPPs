BMS.Fieldbook.SamplesSummaryDataTable = (function ($) {
	var dataTableConstructor = function SamplesSummaryDataTable(tableIdentifier, obsUnitId, plotNumber, programUUID) {
		'use strict';

		var xAuthToken = JSON.parse(localStorage["bms.xAuthToken"]).token;
		var sortKeys = [
			'sampleName',
			'sampleBusinessKey',
			'takenBy',
			'samplingDate',
			'sampleList',
			'plateId',
			'well',
			''
		];

		$(tableIdentifier)
			.on('xhr.dt', function (e, settings, json, xhr) {
				json.recordsTotal = json.recordsFiltered = xhr.getResponseHeader('X-Total-Count');
			})
			.DataTable({
				destroy: true,
				serverSide: true,
				lengthMenu: [10, 50, 75, 100],
				iDisplayLength: 10,
				ajax: {
					url: '/bmsapi/crops/' + cropName + '/programs/' + programUUID + '/samples?obsUnitId=' + obsUnitId,
					dataSrc: '',
					beforeSend: function (xhr) {
						xhr.setRequestHeader('X-Auth-Token', xAuthToken);
					},
					data: function (d) {
						var order = d.order && d.order[0];
						var sort = order && sortKeys[order.column] && (sortKeys[order.column] + ',' + order.dir)

						return {
							size: d.length,
							page: d.length ? d.start / d.length : 0,
							sort: sort || ''
						};
					}
				},
				columns: [
					{data: 'sampleBusinessKey'},
					{data: 'sampleList'},
					{data: 'studyName',
						render: function (data, type, row) {
							if (!data || !data.length || data.length === 0) {
								return '-';
							}
							var authParams =
								'&authToken=' + authToken
								+ '&selectedProjectId=' + selectedProjectId
								+ '&loggedInUserId=' + loggedInUserId;
							return "<a href='/Fieldbook/TrialManager/openTrial/" + row.studyId + "?restartApplication" + authParams
									+ "'>" + data + "</a>"

						}},
					{data: 'takenBy',
						render: function (data) {
							if (!data) {
								return '-';
							}
							return data;
						}
					},
					{data: 'samplingDate',
						render: function (data, type, row) {
							if (!data) {
								return '-';
							}
							if (type === 'sort') {
								return data.split("/").reverse().join();
							}
							return data;
						}
					},
					{data: 'datasetType'},
					{data: 'observationUnitId'},
					{data: 'enumerator'},
					{data: 'plateId'},
					{data: 'well'},
					{data: 'datasets',
						orderable: false,
						render: function (data, type, row) {
							if (!data || !data.length || data.length === 0) {
								return '-';
							}
							var authParams =
								'&authToken=' + authToken
								+ '&selectedProjectId=' + selectedProjectId
								+ '&loggedInUserId=' + loggedInUserId;
							return data.map(function (dataset) {
								return "<a href='/GDMS/main/?restartApplication&datasetId=" + dataset.datasetId
									+ authParams
									+ "'>" + dataset.name + "</a>"
							}).join(", ");
						}
					}
				],
				// TODO server side filtering
				// dom: "<'row'<'col-sm-6'l><'col-sm-6'>f>" +
				//      "<'row'<'col-sm-12'tr>>" +
				//      "<'row'<'col-sm-5'i><'col-sm-7'>>" +
				//      "<'row'<'col-sm-12'p>>"
				dom: "<'row'<'col-sm-6'l><'col-sm-6'>>" +
				     "<'row'<'col-sm-12'tr>>" +
				     "<'row'<'col-sm-5'i><'col-sm-7'>>" +
				     "<'row'<'col-sm-12'p>>"
			});

		if (plotNumber) {
			$(tableIdentifier).closest('.modal').find('.modal-title span').text("(PLOT_NO: " + plotNumber + ")");
		}

	};
	return dataTableConstructor;
})(jQuery);

