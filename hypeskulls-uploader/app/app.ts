import * as fs from "fs";
import csv from "csv-parser";
import { stringify } from "csv-stringify/sync";
import { S3 } from "aws-sdk";
import S3UploadParams from "./Model/S3UploadParams";

const tsvData: any[] = [];
const assetDir = "./assets/VAPOR_ASSETS";
const columnIndex = 0;
const tsvFilePath = "./assets/assets.tsv";
const metadataJsonPath = "./assets/test_metadata.json";
const bucketName = "cbe31cb6-58aa-4633-9e55-5c7190a1dfec-bucket";
const bucketFolder = "vapor_test";

const s3 = new S3({
	apiVersion: "2006-03-01",
	accessKeyId: "G5QjuIw2bQ3I4gDJqQ5fYQ==",
	secretAccessKey: "IFVt4+Z8VSixUDxfs3iVzeIOBuzZJAQSCyxvnfzTcjE=",
	endpoint: "https://storageapi.fleek.co",
	region: "ap-southeast-1",
	s3ForcePathStyle: true
});

async function Main() {

	await processTSVParseAsync(tsvFilePath);
	//await processAssetUpload(tsvData, bucketName);
	writeMetadataJson();
}



async function processAssetUpload(tsvData: any[], bucketName: string) {
	return new Promise(async (resolve, reject) => {

		for (let i = 0; i < tsvData.length; i++) {
			let tsvItem = tsvData[i];
			let baseFileName = getBaseFileName(tsvData[i]);
			console.log(`Processing ${tsvData[i]["tokenName"]}`);
			let files: any[] = tsvItem["files"];

			if (tsvData[i]["files"].length > 0) {
				for (var i2 = 0; i2 < files.length; i2++) {
					try {
						let fileType = files[i2]["mediaType"].split("/").pop();

						if (!checkIsNotNullOrUndefined(files[i2]["src"])) {
							if (fileType != undefined && fileType.length > 0) {

								if (!fs.existsSync(`${assetDir}/${fileType}/${baseFileName}.${fileType}`)) {
									// do something 
									reject(`File does not exist" ${assetDir}/${fileType}/${baseFileName}.${fileType}`);
								}

								let fileStream = fs.createReadStream(`${assetDir}/${fileType}/${baseFileName}.${fileType}`);
								let params: S3UploadParams = {
									Bucket: bucketName,
									Key: `${bucketFolder}/${fileType}/${baseFileName}.${fileType}`,
									ContentType: files[i2]["mediaType"],
									Body: fileStream,
									ACL: "public-read",
								};

								let ipfsHash = await uploadAsync(params);

								while (ipfsHash === null) {
									ipfsHash = await uploadAsync(params);
								}

								fileStream.close();
								files[i2]["src"] = `ipfs://${ipfsHash}`;

								await writeTSVDataAsync(tsvData);
							}
						} else {
							console.log(`${tsvItem["tokenName"]} already uploaded file ${baseFileName}.${fileType} - ${files[i2]["src"]}`);
						}
					}
					catch (error) {
						console.log("An error has occured", error);
					}
				}
			}
			if (!checkIsNotNullOrUndefined(tsvItem["image"])) {

				let fileType = "gif";

				if (!fs.existsSync(`${assetDir}/${fileType}/${baseFileName}.${fileType}`)) {
					// do something 
					reject(`File does not exist" ${assetDir}/${fileType}/${baseFileName}.${fileType}`);
				}

				let fileStream = fs.createReadStream(`${assetDir}/${fileType}/${baseFileName}.${fileType}`);

				let params: S3UploadParams = {
					Bucket: bucketName,
					Key: `${bucketFolder}/${fileType}/${baseFileName}.${fileType}`,
					ContentType: `image/${fileType}`,
					Body: fileStream,
					ACL: "public-read",
				};
				let ipfsHash = await uploadAsync(params);
				while (ipfsHash === null) {
					ipfsHash = await uploadAsync(params);
				}
				fileStream.close();
				tsvItem["image"] = `ipfs://${ipfsHash}`;
				await writeTSVDataAsync(tsvData);

			} else {
				console.log(`${tsvItem["tokenName"]} already uploaded image file - ${tsvItem["image"]}`);
			}
			console.log(`Finished processing ${tsvItem["tokenName"]} \n`);
		}
	});
}

async function writeTSVDataAsync(tsvData: any[]) {
	return new Promise(async (resolve, reject) => {

		if (fs.existsSync(tsvFilePath)) {
			const csv_content = fs.readFileSync(tsvFilePath, "utf8");
			const csv_data = csv_content.split("\n");

			let processedTSVData: any[] = [];
			let tsvDataToProcess = tsvData.map(obj => ({ ...obj }));

			let columns = [];
			if (columnIndex > 0) {
				for (var i = 0; i <= columnIndex; i++) {
					columns.push(csv_data[i]);
				}
			} else {
				columns.push(csv_data[0]);
			}

			for (var i = 0; i < tsvDataToProcess.length; i++) {
				if (checkIsNotNullOrUndefined(tsvDataToProcess[i]["files"])) {
					if (tsvDataToProcess[i]["files"].length > 0) {

						let mediaType: string[] = [];
						let src: string[] = [];
						let files: any[] = tsvDataToProcess[i]["files"];
						files.forEach(fileItem => {
							mediaType.push(fileItem["mediaType"]);
							src.push(fileItem["src"]);
						});

						tsvDataToProcess[i]["mediaType"] = mediaType.join(",");
						tsvDataToProcess[i]["src"] = src.join(",");
						delete tsvDataToProcess[i]["files"];
					}
				}
				processedTSVData.push(tsvDataToProcess[i]);
			}

			const stringifiedData = stringify(processedTSVData, { delimiter: "\t", columns: columns[columnIndex].split("\t") });

			let tsv_content = columns.join("\n") + "\n" + stringifiedData;
			fs.writeFileSync(tsvFilePath, tsv_content);
			console.log(`TSV file updated.`, "\n");
			resolve(null);
		}
		else {
			reject("TSV file not found")
		}

	});
}

async function uploadAsync(params: S3UploadParams) {
	return new Promise<string>((resolve, reject) => {
		console.log(`Preparing to upload ${params.Key} }}`);
		const request = s3.putObject(params);
		console.log(`Uploading: ${params.Key}`);

		let ipfsHashV0 = "";

		request.on("httpHeaders", (statusCode, headers) => {
			ipfsHashV0 = headers["x-fleek-ipfs-hash-v0"];
			console.log(`IPFS Hash V0 of ${params.Key} - ${ipfsHashV0}`);
		}).
			on("success", function (response) {
				console.log(`Successfully uploaded - ${params.Key}`);
			}).
			on("error", function (error, response) {
				console.log(`Error occured failed to upload - ${params.Key}`);
				reject(error);
			}).
			on("complete", function (response) {
				console.log("\n");
				resolve(ipfsHashV0);
			}).send();
	})
};

async function processTSVParseAsync(csvPath: string) {
	return new Promise((resolve, reject) => {
		console.log("retrieving TSV Data");

		if (fs.existsSync(tsvFilePath)) {
			fs.createReadStream(csvPath)
				.pipe(csv({ skipLines: 0, separator: "\t" }))
				.on("data", (data) => {
					data["mediaType"] = data["mediaType"].split(",");
					data["src"] = data["src"].split(",");
					data["files"] = [];
					for (var i = 0; i < data["mediaType"].length; i++) {
						let fileItem: any = {};
						fileItem["mediaType"] = data["mediaType"][i];
						fileItem["name"] = data["name"];
						if (checkIsNotNullOrUndefined(data["src"][i])) {
							fileItem["src"] = data["src"][i];
						} else {
							fileItem["src"] = "";
						}
						data["files"].push(fileItem);
					}

					delete data["mediaType"];
					delete data["src"];
					tsvData.push(data)
				})
				.on("error", (error) => {
					reject(error);
				})
				.on("end", () => {
					resolve(null);
				});
		} else {
			reject("TSV file not found")
		}
	});
}

function getBaseFileName(tsvItem: any): string {

	if (checkIsNotNullOrUndefined(tsvItem["type"]) && checkIsNotNullOrUndefined(tsvItem["rarity"])) {
		return `VAPOR_${tsvItem["rarity"]}_${tsvItem["type"]}`.toUpperCase();
	}

	return "";
}

function checkIsNotNullOrUndefined(data: any) {
	return data !== undefined && data !== null && data.length > 0;
}

function writeMetadataJson() {

	let tsvDataToProcess : any = {};
	
	for (let i = 0; i < tsvData.length; i++) {
		let tsvItem = tsvData[i];
		tsvItem["mediaType"] = tsvItem["imageMediaType"];
		tsvDataToProcess[tsvItem["tokenName"]] = { ...tsvItem };
		delete tsvDataToProcess[tsvItem["tokenName"]]["tokenName"];
		delete tsvDataToProcess[tsvItem["tokenName"]]["totalSupply"];
		delete tsvDataToProcess[tsvItem["tokenName"]]["imageMediaType"];
	}

	let metadata = {
		"721": {
			"<policy_id>": tsvDataToProcess
		},
		"version": "1.0"
	}

	fs.writeFileSync(metadataJsonPath, JSON.stringify(metadata, null, "\t"));
}

(async () => {
	try {
		await Main();
	} catch (err) {
		console.error(err);
		process.exit(1);
	}
})();