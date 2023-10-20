# zoning-update

Creates a workflow for a DevOps system for layers and applications in ArcGIS Online.

The environments have a Geodatabase where the data is stored and created, a Folder of hosted feature layers, and applications.

There are two environments that represent the DEV (Development) and Prod (Production)
The DEV environment is where all work/ development gets done, once the changes in DEV are considered stable and approved for use in the client facing application, the DEV environment is pushed to Prod.
Each environment has parallels between their data sources in a geodatabase on the W: Drive as well as Hosted feature layers and application on ArcGIS Online. The DEV versions of the assets are marked with the _DEV suffix. The Prod assets have the same name as the DEV assets without the _DEV suffix. 
When you are ready to push an update to the production environment the push_to_prod() function can be run. This will prompt you for the username and password. Note that this login needs to be the same as the owner of the applications. 


## Setting up environments:
Each application will have a DEV and Prod version of each asset. Original Feature classes and tables created and managed in ArcGIS pro should have a prod geodatabase and a DEV copy with the _DEV suffix. The Hosted feature layers and application being used in an application should be placed in project specific folders, Prod with the project_name and DEV with project_name_DEV. The DEV Feature layers and Apps should also be shared to the ALL_DEV Group for developers to access, the Prod Feature layers and Apps can be shared to whichever relevant groups are needed for the app.
