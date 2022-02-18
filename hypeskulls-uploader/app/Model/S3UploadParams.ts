import * as fs from 'fs';
type S3UploadParams = {
	Bucket: string
	Key: string,
	ContentType: string;
	Body: fs.ReadStream;
	ACL: string;
}
export default S3UploadParams;