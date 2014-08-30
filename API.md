# Blog-Core API

## Users

### Authenticate

Request URL: `/api/auth`, type `POST`.

Fields:

 * username
 * password

Returned fields:

 * id
 * type
 * key

Error responses:

 * Invalid auth credentials.
 * Invalid data.

### Add a new user

Request URL: `/api/user`, type `POST`.

Fields:

 * username - must match email address pattern
 * fullname - must be set
 * password - minimum length 6, optional
 * type - either admin or author
 * link - user link, can be empty string
 * files - boolean whether the user can manage files

Returns user id.
Requires the current user to be an admin.

Error responses:

 * Invalid or missing API key.
 * The user password is not set.
 * The username is not an email address.
 * The username exists.
 * The operation requires admin privileges.
 * Invalid data.

### Retrieve the existing user

Request URL: `/api/user/Id`, type `GET`.

Returned fields:

 * $id
 * username
 * fullname
 * type
 * link
 * files

Requires the current user to be an admin.

Error responses:

 * Invalid or missing API key.
 * The operation requires admin privileges.
 * The user does not exist.

### Modify the existing user

Request URL: `/api/user/Id`, type `PUT`.

Fields:

 * username - must match email address pattern
 * fullname - must be set
 * password - minimum length 6, optional
 * type - either admin or author
 * link - user link, can be empty string
 * files - boolean whether the user can manage files

Returns user id.
Requires the current user to be an admin.
When password is not set then it is not changed.

Error responses:

 * Invalid or missing API key.
 * The username is not an email address.
 * The username exists.
 * The operation requires admin privileges.
 * Invalid data.
 * Cannot demote the last admin.
 * The user does not exist.

### Remove the existing user

Request URL: `/api/user/Id`, type `DELETE`.

Returns the removed user id.
Requires the current user to be an admin.

Error responses:

 * Invalid or missing API key.
 * The operation requires admin privileges.
 * Cannot remove the last admin.
 * The user does not exist.
 * The user has posts.

### List of users

Request URL: `/api/users`, type `GET`.

Returned fields:

 * $id
 * username
 * fullname
 * type

Requires the current user to be an admin.

Error responses:

 * Invalid or missing API key.
 * The operation requires admin privileges.

## Entries (posts)

### Add a new post

Request URL: `/api/entry`, type `POST`.

Fields:

 * title
 * slug
 * tags (array of strings)
 * date_published (optional)
 * date_updated
 * commenting (boolean)
 * published (boolean)
 * content
 * content_type (enum markdown/raw)
 * description
 * type (enum post/page/block)
 * language (language identifier)

Requires authentication.
Returns the saved post id.
Author of the post is set from the current user.

Error responses:

 * The slug already exists.
 * Invalid data.
 * Invalid or missing API key.

### Retrieve the existing post

Request URL: `/api/entry/Id`, type `GET`.

Returned fields:

 * $id
 * slug
 * type
 * date_published
 * date_updated
 * commenting
 * published
 * title
 * author
 * content
 * description
 * content_type
 * tags
 * comments (comments count)

Requires authentication.

Error responses:

 * The post does not exist.
 * Invalid or missing API key.

### Retrieve the existing post info

Request URL: `/api/entry/Id/info`, type `GET`.

Same as `/api/entry/Id` but does not retrieve the content field.

### Update the existing post

Request URL: `/api/entry/Id`, type `PUT`.

Fields:

 * author
 * title
 * slug
 * tags (array of strings)
 * date_published (optional)
 * date_updated
 * commenting (boolean)
 * published (boolean)
 * content
 * content_type (enum markdown/raw)
 * description
 * type (enum post/page/block)
 * language (language identifier)

Requires authentication.
Author can be modified only when current user is admin.
Non-admin can only modify own posts.

Error responses:

 * The post does not exist.
 * Invalid or missing API key.
 * The slug already exists.
 * The operation requires admin privileges.
 * Invalid data.

### Remove the existing entry

Request URL: `/api/entry/Id`, type `DELETE`.

Returns the removed entry id.
Entry comments are removed.
Non-admin user can only remove own entry.
Requires authentication.

Error responses:

 * The post does not exist.
 * Invalid or missing API key.
 * The operation requires admin privileges.

### List of entries

Request URL: `/api/entries/Type`, type `GET`.

The type is one of: `post`, `page`, `block`.

Returned fields:

 * $id
 * slug
 * type
 * date_published
 * date_updated
 * commenting
 * published
 * title
 * author

Requires authentication.

Error responses:

 * Invalid or missing API key.

Type is not validated at the moment. Empty array will be returned on
non-existent type.

## Comments

### Add a new comment

Request URL: `/api/post/Id/comment`, type `POST`.

Fields:

 * author
 * email (optional)
 * site (optional)
 * comment
 * content
 * reply_to (optional)
 * question (integer)
 * answer

This endpoint does not require authentication.

Error responses:

 * The human question answer is wrong.
 * Commenting is disabled for the entry.
 * The comment replied to does not exist.
 * The entry does not exist.
 * Invalid data.

### Tree of comments

Request URL: `/api/post/Id/comments`, type `GET`.

Returned fields:

 * author
 * email (optional)
 * site (optional)
 * comment
 * content
 * reply_to (optional)
 * comments (array of replies)

The endpoint requires authentication.
Comments in a single node are ordered by date in descending order.

Error responses:

 * Invalid or missing API key.
 * The entry does not exist.

### Remove the existing comment

Request URL: `/api/comment/Id`, type `DELETE`.

The endpoint requires authentication.
Non-admin user can remove only own post comments.
Returns the removed comment id.
Comment replies will be removed too.

Error responses:

 * Invalid or missing API key.
 * The comment does not exist.
 * The operation requires admin privileges.

### Human check question

Request URL: `/api/question`, type `GET`.

Returned fields:

 * question (string)
 * id (number)

Does not require authentication and has no error responses.

## Configuration entries

### List of entries

Request URL: `/api/configs`, type `GET`.

Returned fields:

 * name
 * value

Requires authentication and admin privileges.

Error responses:

 * Invalid or missing API key.
 * The operation requires admin privileges.

### Update the config entry

Request URL: `/api/configs`, type `PUT`.

Fields:

 * name
 * value

Requires authentication and admin privileges.

Error responses:

 * Invalid or missing API key.
 * The operation requires admin privileges.
 * Invalid data.

If the config entry does not exist then it will be added.
