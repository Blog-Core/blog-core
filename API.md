# Blog-Core API

## Users

### Authenticate

Request URL: /api/auth, type POST.

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

Request URL: /api/user, type POST.

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

 * The user password is not set.
 * The username is not an email address.
 * The username exists.
 * The operation requires admin privileges.
 * Invalid data.

### Retrieve the existing user

Request URL: /api/user/Id, type GET.

Returned fields:

 * $id
 * username
 * fullname
 * type
 * link
 * files

Requires the current user to be an admin.

Error responses:

 * The operation requires admin privileges.
 * The user does not exist.

### Modify the existing user

Request URL: /api/user/Id, type PUT.

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

 * The username is not an email address.
 * The username exists.
 * The operation requires admin privileges.
 * Invalid data.
 * Cannot demote the last admin.
 * The user does not exist.

### Removing the existing user

Request URL: /api/user/Id, type DELETE.

Returns the removed user id.
Requires the current user to be an admin.

Error responses:

 * The operation requires admin privileges.
 * Cannot remove the last admin.
 * The user does not exist.
 * The user has posts.

### List of users

Request URL: /api/users, type GET.

Returned fields:

 * $id
 * username
 * fullname
 * type

Requires the current user to be an admin.

Error responses:

 * The operation requires admin privileges.
