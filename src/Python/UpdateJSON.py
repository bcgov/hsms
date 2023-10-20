import json
import os
import arcpy
import pandas as pd
from arcgis.gis import GIS
import getpass

# Creates a workflow for a DevOps system for layers and applications in ArcGIS Online.
# Use uploadAgolLayers() to upload/overwrite a geodatabase on its own
# Use push_to_prod() to push the dev environment to the prod environment

def login():
    """Logs User in to Arcpy tools and to their ArcGIS Online account.

    Returns the gis object for ArcGIS API for Python.
    """
    global user 
    
    print("Please login to ArcGIS Online")
    
    while True:
        try:      
            username=input("Username: ") #ArcGIS Online Username
            password = getpass.getpass(prompt="Password: ") #ArcGIS Online Password

            gis = GIS("https://www.arcgis.com", username, password) #gis object for ArcGIS API for Python
            break
            
        except (ValueError, Exception) as e:
            print("Incorrect Username or password. Try again")
    
    user = gis.users.me #user object to point to arcgis online account currently logged in
    arcpy.SignInToPortal("https://www.arcgis.com", username , password)
    print("Signed in Sucessfully")

    return gis

def backup_gdp(dev_gdb, prod_gdb):
    """backup_gdb(dev_gdb, prod_gdb)
    
    Backs up prod geodatabase to a backup directory, then pushes dev gdb to prod gdb

    Parameters:
    dev_gdb (str): file path to development geodatabase
    prod_gdb (str): file path to production geodatabase
    """
    
    # get file path for backup gdb
    directory = os.path.dirname(prod_gdb)
    backup_dir = os.path.join(directory, "Backup")
    file_name = os.path.basename(prod_gdb)
    backup_gdp = os.path.join(backup_dir,file_name)
    
    # copy prod gdb to dev
    arcpy.env.workspace = backup_gdp
    arcpy.env.overwriteOutput = True
    arcpy.Copy_management(prod_gdb,backup_gdp)

    # copy dev gdb over prod gdb
    arcpy.env.workspace = prod_gdb
    arcpy.env.overwriteOutput = True
    arcpy.Copy_management(dev_gdb,prod_gdb)

def dev_widget_info(dev_app):
    """dev_widget_info(dev_app)
    
    Gets information on Widgets in an application and populates the global ref_table.
    Each row represents a widget in the app that isn't a map or richtext

    Parameters:
    dev_app(str): id of dev application in ArcGIS Online

    Returns the dev json dictionary 
    """
    
    global ref_table
    global columns

    print("Getting DEV Json data")
    
    # go down dev json, get names and ids and layerids for every widget
    dev_app_item = gis.content.get(dev_app)
    dev_json = dev_app_item.get_data() # dictionary from ArcGIS Online app json
    # Get widget count
    num_widgets = len(dev_json["desktopView"]["widgets"]) #number of widgets in app

    # Iterate through every widget
    for widget in range(num_widgets):
        widget_name = dev_json["desktopView"]["widgets"][widget]["id"]
        widget_type = dev_json["desktopView"]["widgets"][widget]["type"]
        
        # Only widget types that contain a data source will populate the table
        if widget_type != "mapWidget" and widget_type != "richTextWidget":
            widget_id = dev_json["desktopView"]["widgets"][widget]["datasets"][0]["dataSource"]["itemId"]

            new_data = [[widget_name, widget_type, widget_id, None, None]] # data to add to ref_table
            new_row = pd.DataFrame(new_data, columns=columns) # data frame with one row to concatenate to ref_table

            ref_table = pd.concat([ref_table,new_row], ignore_index=True) #add new row to ref_table
    
    return dev_json

def get_dev_data():
    """get_dev_data()
    
    Gets the Title of all of the features layers in the table from their id.
    Note that the Feature name will not contain the _DEV suffix since it is the title of the layer not the Hosted Feature."""
    
    global ref_table
    
    print("Getting Dev Feature Info")

    # iterate through ref_table to get id of the dev data sources
    for index, value in ref_table["DEV itemID"].items():
        #source_data = ref_table.loc[index,"DEV itemID"]
        
        item = gis.content.get(str(value)) # dev data source used in widget
        layers = item.layers
        
        name = layers[0].properties.name #layer name of Hosted feature layer
        ref_table.loc[index,"Feature Name"] = name

def uploadAgolLayers(folder, workspace, isDev = False, share_groups = None, new_tag = None, new_summary = "EMPTY", new_description = "EMPTY", source_field = None):
    """uploadAgolLayers(folder,workspace, {isDev}, {share_group}, {new_tag}, {new_summary}, {new_description})
    
    Bulk uploads Feature Classes from a geodatabase to ArcGIS Online as a hosted feature layer

    Parameters:
    folder(str): Folder in ArcGIS Online where layers will be uploaded
    workspace(str): filepath to geodatabase where feature classes are located all featureclasses will be uploaded
    isDev(bool): True if workspace is a Dev geodatabase, if True DEV tag will be added. Default is False
    share_groups(list): Name of group to share the feature classes to. Default is None
    new_tag (str): tag for the hosted feature layer. Default is None
    new_summary (str): summary for the hosted feature layer. Default is None
    new_description (str): description for the hosted feature layer. Default is None
    source_field (str): Name of source field in Feature Classes
    """
    
    global user
    
    aprx = arcpy.mp.ArcGISProject(r"W:\mtic\vic\rpd\Workarea\Working\EBORTHIS\AGOL UPLOAD\AGOL UPLOAD.aprx")
    m = aprx.listMaps('UPLOAD LAYERS')[0]

    #Remove all layers from map before running
    for lyr in m.listLayers():        
        m.removeLayer(lyr)
    aprx.save()

    folders = user.folders
    folder_names = [folder['title'] for folder in folders]

    if folder in folder_names:
        newFolder = False
    else:
        newFolder = True

    outdir = r"W:\mtic\vic\rpd\Workarea\Working\EBORTHIS\AGOL UPLOAD\SCRATCH"
    #Go through folder and add a tag to each feature class item
    arcpy.env.workspace = workspace
    arcpy.env.overwriteOutput = True
    # add all fc in gdb to map
    feature_list = arcpy.ListFeatureClasses()
    num_fc = len(feature_list)

    for fc in feature_list:
        layerResult = arcpy.MakeFeatureLayer_management(fc, fc)
        m.addLayer(layerResult[0])
        aprx.save()

    fc_count = 1
    #loop through map layers
    for lyr in m.listLayers():
        
        source_found = True
        if source_field != None:
            # Use a SearchCursor to retrieve the value
            try:
                with arcpy.da.SearchCursor(lyr, source_field) as cursor:
                    for row in cursor:
                        data_source = row[0]
                        break  # Exit the loop after retrieving the first row
            except RuntimeError as e:
                if 'Cannot find field' in str(e):
                    source_found = False
                    print(f"The field '{source_field}' does not exist in the feature class.")
        
        #prepare service definition
        if not isDev:
            service_name = lyr.name
        else:
            service_name = lyr.name + "_DEV"
        sddraft_filename = service_name + ".sddraft"
        sddraft_output_filename = os.path.join(outdir, sddraft_filename)
        sd_filename = service_name + ".sd"
        sd_output_filename = os.path.join(outdir, sd_filename)
        
        arcpy.AddMessage(f"Preparing Layer {fc_count} of {num_fc}: {service_name}")

        # Create FeatureSharingDraft and set overwrite property
        sddraft = m.getWebLayerSharingDraft("HOSTING_SERVER", "FEATURE", service_name, lyr)
        sddraft.overwriteExistingService = True

        # Create Service Definition Draft file
        sddraft.exportToSDDraft(sddraft_output_filename)

        # Stage Service
        arcpy.AddMessage("Staging: " + service_name + "...")
        arcpy.StageService_server(sddraft_output_filename, sd_output_filename)

        # Share to portal
        arcpy.AddMessage("Uploading...")
        
        if newFolder:
            arcpy.UploadServiceDefinition_server(sd_output_filename, "HOSTING_SERVER", in_folder_type="NEW", in_folder= folder)
        else:
            arcpy.UploadServiceDefinition_server(sd_output_filename, "HOSTING_SERVER", in_folder_type="EXISTING", in_folder= folder)

        arcpy.AddMessage("Finished Publishing " + service_name)
        # Retrieve items from a specific folder
        folder_items = user.items(folder=folder, max_items=1000)
        
        group_list = []
        if share_groups != None:
            for group_name in share_groups:
                group = gis.groups.search(group_name)[0]
                group_id = group.id
                group_item = gis.groups.get(group_id)
                group_list.append(group_item)

        for item in folder_items:
            if item.type == "Feature Service" and item.title == service_name:
                if new_tag != None:
                    if item.tags == [""]:
                        item.update(item_properties={"tags": new_tag})
                    else:
                        tags = item.tags
                        tags.append(new_tag)
                        item.update(item_properties={"tags": tags})
                if source_field != None and source_found:
                    if item.tags == [""]:
                        item.update(item_properties={"tags": data_source})
                    else:
                        tags = item.tags
                        tags.append(data_source)
                        item.update(item_properties={"tags": tags})
                if isDev:
                    if "DEV" not in item.tags:
                        tags = item.tags
                        tags.append("DEV")
                        item.update(item_properties={"tags": tags})
                if new_summary != "EMPTY":
                    item.update(item_properties={"snippet": new_summary})
                if new_description != "EMPTY":
                    item.update(item_properties={"description": new_description})
                
                item.share(groups=group_list, allow_members_to_edit =True)
        
        fc_count += 1

    arcpy.AddMessage("Layers Uploaded")

def get_prod_data(folder_name:str):
    """get_prod_data(folder_name)
    
    Gets the matching prod feature layer and places it into ref_table.
    
    Parameters:
    folder_name (str): name of folder where prod data is located
    """

    global ref_table
    global user

    print("Getting Prod Data")
    
    folder_items = user.items(folder=folder_name, max_items=1000) # get all items in folder

    # iterate through rows in ref table to find matching prod feature
    for index, value in ref_table["Feature Name"].items():
        for item in folder_items:
            title = item.title #title of prod feature layer
            #title = title.replace("_DEV", "")
            #value is the Feature name in ref_table for the current row
            if item.type == "Feature Service" and title == value:
                ref_table.loc[index,"PROD itemID"] = item.id

def backup_json(prod_app, prod_name, backup_directory):
    """backup_json(prod_app, prod_name, backup_directory)

    write prod json to a specified back folder

    Parameters:
    prod_app(str): id of production app in ArcGIS Online
    prod_name(str): name of production App
    backup_directory(str): filepath to json backup folder
    """

    prod_app_item = gis.content.get(prod_app)
    prod_json = prod_app_item.get_data()# dictionary from ArcGIS Online app json 
    
    file_name = prod_name + ".json"
    backup_file = os.path.join(backup_directory, file_name) #backup json file path 
    out_file = open(backup_file, "w")
    json.dump(prod_json, out_file, indent=6) #write json to file

def update_prod_json(prod_app , dev_json):
    """update_prod_json(prod_app, dev_json)
    
    pushes update from the dev apps json to prod and transfers datasources to prod sources

    Parameters:
    prod_app(str): id of production ArcGIS Online application
    dev_json(dict): dictionary representation of dev app json 
    """
    
    global ref_table
    
    print("Updating Prod Json")

    prod_json = dev_json

    # Get widget count
    num_widgets = len(prod_json["desktopView"]["widgets"])

    # Iterate through every widget
    for widget in range(num_widgets):
        widget_name = prod_json["desktopView"]["widgets"][widget]["id"]
        for index, value in ref_table["Widget Name"].items():
            if widget_name == value:
                #replace widget itemId with prod itemID
                prod_json["desktopView"]["widgets"][widget]["datasets"][0]["dataSource"]["itemId"] = ref_table.loc[index,"PROD itemID"]
                prod_json["desktopView"]["widgets"][widget]["datasets"][0]["dataSource"]["layerId"] = 0

    
    out_json = json.dumps(prod_json)
    
    prod_app_item = gis.content.get(prod_app)
    prod_app_item.update(data=out_json)

def transition_to_individual():
    '''Deprecated'''
    global ref_table
    global columns
    app_csv = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Documents\HSMS App IDs.csv"
    app_list = pd.read_csv(app_csv)
    columns = ["Widget Name", "Widget Type", "DEV itemID", "DEV layerID", "Feature Name", "PROD itemID"]
    
    #loop through dev and prod apps from table
    for index, row in app_list.iterrows():
        ref_table = pd.DataFrame(columns=columns)
        dev_app = row["Dev ID"]
        prod_app = row["Prod ID"]
        prod_name = row["Name"]
        arcpy.AddMessage(f"Updating {prod_name}")

        dev_json = dev_widget_info(dev_app)
        get_dev_data()
        get_prod_data("HSMS_DEV")
        update_prod_json(prod_app, dev_json)

def push_to_prod(app_csv, backup_directory):
    """push_to_prod(app_csv, backup_directory)
    
    Pushes development databases and apps to production environment.

    Parameters:
    app_csv (str): path to table with columns Name, Dev ID and Prod ID. ID columns have ArcGIS Online IDs for applications
    backup_directory (str): filepath to json backup folder
    """
    global ref_table
    global gis
    global columns

    columns = ["Widget Name", "Widget Type", "DEV itemID", "Feature Name", "PROD itemID"]

    statsCan_gdb_dev = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Geodatabases\StatsCan_DEV.gdb"
    statsCan_gdb = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Geodatabases\StatsCan.gdb"
    cmhc_gdb_dev = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Geodatabases\CMHC_DEV.gdb"
    cmhc_gdb = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Geodatabases\CMHC.gdb"
    hous_gdb_dev = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Geodatabases\HOUS_DEV.gdb"
    hous_gdb = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Geodatabases\HOUS.gdb"
    
    gis = login()
    
    gdb_list = []
    #ask user which gdb's they would like to update
    while True:
        choice = input("Would you like to update the StatsCan gdb? Y or N: ").lower()
        
        if choice == "y":
            gdb_list.append((statsCan_gdb_dev, statsCan_gdb))
            break
        elif choice == "n":
            break
        else:
            print("Invalid input, please enter Y or N")
    
    while True:
        choice = input("Would you like to update the cmhc gdb? Y or N: ").lower()
        
        if choice == "y":
            gdb_list.append((cmhc_gdb_dev, cmhc_gdb))
            break
        elif choice == "n":
            break
        else:
            print("Invalid input, please enter Y or N")

    while True:
        choice = input("Would you like to update the hous gdb? Y or N: ").lower()
        
        if choice == "y":
            gdb_list.append((hous_gdb_dev, hous_gdb))
            break
        elif choice == "n":
            break
        else:
            print("Invalid input, please enter Y or N")

    #backup and copy dev gdbs to prod
    for gdb in gdb_list:
        dev_gdb, prod_gdb = gdb
        arcpy.AddMessage(f"Updating {prod_gdb}")
        backup_gdp(dev_gdb,prod_gdb) 
        #update layers
        uploadAgolLayers("HSMS_PROD",prod_gdb, new_tag="HSMS",share_groups=["HSMS Content","HSMS Prod", "OHCS"], source_field="Data_source")

    app_list = pd.read_csv(app_csv)

    #iterate through apps pushing dev data and apps to prod
    for index, row in app_list.iterrows():
        ref_table = pd.DataFrame(columns=columns)
        dev_app = row["Dev ID"]
        prod_app = row["Prod ID"]
        prod_name = row["Name"]
        
        arcpy.AddMessage(f"Updating {prod_name}:")
        
        dev_json = dev_widget_info(dev_app)
        get_dev_data()
        get_prod_data("HSMS_PROD")
        backup_json(prod_app, prod_name, backup_directory)
        update_prod_json(prod_app, dev_json)
        arcpy.AddMessage("")

def revert_app(app_name, backup_directory):
    """revert_app(app_name, backup_directory)

    Reverts Production applications to previous version

    Parameters:
    app_name (str): Name of Application
    backup_directory (str): filepath to json backup folder
    """
    
    file_name = app_name + ".json"
    backup_file = os.path.join(backup_directory, file_name)

    with open(backup_file) as json_file:
        backup_json = json.load(json_file)
    
    app_csv = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Documents\HSMS App IDs.csv"
    app_list = pd.read_csv(app_csv)

    for index, row in app_list.iterrows():
        prod_app = row["Prod ID"]
        prod_name = row["Name"]

        if prod_name == app_name:
            app_id = prod_app
            break
    
    out_json = json.dumps(backup_json)
    
    app_item = gis.content.get(app_id)
    app_item.update(data=out_json)

def revert_gdb(gdb_dir, backup_directory):
    """revert_gdb(gdb_dir, backup_directory)

    Reverts a prod gdb back to its previous version

    Parameters:
    gdb_dir(str): directory of the geodatabase
    backup_directory(str): directory where gdb backups are kept
    """

    backup_gdb_name = os.path.basename(gdb_dir)
    backup_gdb = os.path.join(backup_directory,backup_gdb_name)

    arcpy.Copy_management(backup_gdb,gdb_dir)

    uploadAgolLayers("HSMS_PROD",gdb_dir, new_tag="HSMS",share_groups=["HSMS Content","HSMS Prod", "OHCS"], source_field="Data_source")

#stats_gdb = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Geodatabases\StatsCan_DEV.gdb"
#cmhc_gdb = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Geodatabases\CMHC_DEV.gdb"
#hous_gdb = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Geodatabases\HOUS.gdb"
#muni_gdb = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Geodatabases\Municipality_Targets.gdb"
#prod_gdb = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Geodatabases\StatsCan_DEV.gdb"

#gis = login()
#uploadAgolLayers("HSMS_DEV",dev_gdb, isDev=True, new_tag="HSMS", share_groups=["HSMS Dev"], source_field="data_source")

app_csv = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Data\Documents\HSMS App IDs.csv"
backup_directory = r"W:\mtic\vic\rpd\Workarea\ArcGIS_Online\OHCS\Analysis\Git\HSMS\hsms\res\Apps"
push_to_prod(app_csv, backup_directory)