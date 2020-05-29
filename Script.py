# -*- coding: utf-8 -*-
"""
Created on Mon May 11 12:09:28 2020

@author: zacha
"""


####  Needs proper heading 
####  This is my work on top of K. Jones xrray grab for netcdfs from the maca threads
####  Authors K. Jones, Z. Robbins 2020 ***Pandemic Code***



### Below I am reading in individual URL's from NKN Thredd Server, however, this could be modified to take in the text file produced 
### by the MACA portal download and 
### read the url list within the text file. Since the URLs can be generated to download netcdfs at a smaller bounding 
### box extent (from the maca website), it would eliminate the need to do the
### "slicing" operation in the loop at the bottom of this script.
### This set can only handle variables for Tmin,Tmax,RhMax,RhMin,WindSpeed and
### Direction. 

##User Inputs 
Drive='C:/Users/zacha/Documents/GitHub/MACA_climate_forLANDIS/'


### This drive needs the following paths 
### Inputs/
    ## This should contain a file from the Maca website
    ## And Climate_Regions/ which contain complete polygons for climate 
    ## regions
### Temp/ (empty)
### Outputs/ (empty)
### End inputs. 


## Librarys
import time,os
import geopandas as gp
import pandas as pd
from functools import partial



os.chdir(Drive)
import MACA_FunctionBank as MFB
### This require the modules:
    #xarray,sys,numpy,netCDF4,datetime,geopandas,rasterio,pandas,functools,scipy

###  Get the text file from. 
###  https://climate.northwestknowledge.net/MACA/data_portal.php
###  Name of the model in the textfile we will bring in. 
Modelname="GFDL-ESM2M"   

##Shapefile names we will bring in. Should be in projection of _____ as the 
## MACA Netcdf is. 
shapefilesnames=["eco1","eco2","eco3","eco4","eco5","eco6","eco8","eco9",
             "eco10","eco11"]

### Name names used within the final LANDIS File
### in the Sapps Model eco1 is the off ecoregion
EcoregionBeginswith=2 
EcoregionEndswith=11

#Inputs
### Set a reasonable bounding box for the first cut. (smaller will increase speed)
lonmin=274.4312 
lonmax=279.8369 
latmin=33.9681
latmax=37.0724

###Paths
path='Inputs/Climate_Regions/'
filepath =Drive+ 'Inputs/macav2_OneModel_Test.txt'

###Check your files 
Ecos,Modelfiles=MFB.ModelParser(Drive,Modelname,filepath,EcoregionBeginswith,
                                EcoregionEndswith,path,shapefilesnames)

print("Loading in NC data for the variable size (Note this will require a large",
      ">10Gb available C: memory. Memory will be utilized and then cleared.")



### Create a partial (ei a model with all but one varaible for parallel processing)
partLoadandCut=partial(MFB.Loadinandcut,Drive=Drive,latmin=latmin,latmax=latmax,lonmin=lonmin,lonmax=lonmax)

for i in Modelfiles:
        start1=time.time()
        print("Loading in Data from host and cutting into temp",time.time()-start1,)
        partLoadandCut(httpline=i)
        print("DataLoaded",time.time()-start1)

for i in Modelfiles:
        print(i)
        
        start1=time.time()
        httpline=i
        print("Preparing Variables",time.time()-start1)
        ### Set up lookups 
        trimmed_name = httpline.split('macav2metdata_')[1]
        #print(trimmed_name)
        modelname = trimmed_name.split('_daily')[0]
        #print(modelname)
        variablename=modelname.split('_')[0]
        #print(variablename)
        ### 
        NC_path=(Drive+"Temp/"+modelname+".nc")
        par=variablename
        #print(par)
        if "pr" == par:
            key='precipitation'
            #model=file[16:]
            #model=model.replace('_2006_2099_CONUS_daily_aggregated.nc',"")
        if "tasmin"== par:
            key='air_temperature'
            #model=file[20:]
            #model=model.replace('_2006_2099_CONUS_daily_aggregated.nc',"")      
        if "tasmax" == par:
            key='air_temperature' 
            #model=file[20:]
            #model=model.replace('i1p1_rcp85_2006_2099_CONUS_daily_aggregated.nc',"")
        if"rhsmax"== par:
            key='relative_humidity'
        if"rhsmin"== par:
            key='relative_humidity' 
        if"vpd"== par:    
            key='vpd'
        if "uas"== par:
            key="eastward_wind"
        if "vas"== par:
            key="northward_wind"
        
        
        Template,dates=MFB.FormatNetCdf(Drive,NC_path, key)
        
        ### masking and writing matrices
        
        ### !! This section should be a looping function at some point which
        ### !! passes outputs to a labeled dictionary 
        ### !! Then loop the dictionary into the dataframe. 
        
        print("Cutting_Begins",time.time() - start1)

        Shape1= gp.read_file((Drive+'Inputs/Climate_regions/'+shapefilesnames[0]+'.shp'))
        Shape2= gp.read_file((Drive+'Inputs/Climate_regions/'+shapefilesnames[1]+'.shp'))
        Shape3= gp.read_file((Drive+'Inputs/Climate_regions/'+shapefilesnames[2]+'.shp'))
        Shape4= gp.read_file((Drive+'Inputs/Climate_regions/'+shapefilesnames[3]+'.shp'))
        Shape5= gp.read_file((Drive+'Inputs/Climate_regions/'+shapefilesnames[4]+'.shp'))
        Shape6= gp.read_file((Drive+'Inputs/Climate_regions/'+shapefilesnames[5]+'.shp'))
        Shape8= gp.read_file((Drive+'Inputs/Climate_regions/'+shapefilesnames[6]+'.shp'))
        Shape9= gp.read_file((Drive+'Inputs/Climate_regions/'+shapefilesnames[7]+'.shp'))
        Shape10= gp.read_file((Drive+'Inputs/Climate_regions/'+shapefilesnames[8]+'.shp'))
        Shape11= gp.read_file((Drive+'Inputs/Climate_regions/'+shapefilesnames[9]+'.shp'))
        practiceoneeco1=MFB.MaskandWrite(Shape1,key,Template,dates,"eco2")
        practiceoneeco2=MFB.MaskandWrite(Shape2,key,Template,dates,"eco3")
        practiceoneeco3=MFB.MaskandWrite(Shape3,key,Template,dates,"eco4")
        practiceoneeco4=MFB.MaskandWrite(Shape4,key,Template,dates,"eco5")
        practiceoneeco5=MFB.MaskandWrite(Shape5,key,Template,dates,"eco6")
        practiceoneeco6=MFB.MaskandWrite(Shape6,key,Template,dates,"eco7")
        practiceoneeco7=MFB.MaskandWrite(Shape8,key,Template,dates,"eco8")
        practiceoneeco8=MFB.MaskandWrite(Shape9,key,Template,dates,"eco9")
        practiceoneeco9=MFB.MaskandWrite(Shape10,key,Template,dates,"eco10")
        practiceoneeco10=MFB.MaskandWrite(Shape11,key,Template,dates,"eco11")
  

        ### Merging one dataframe this could be cleaned 
        stepone=pd.merge(practiceoneeco1,practiceoneeco2,how='inner', on='Timestep')
        steptwo=pd.merge(stepone,practiceoneeco3,how='inner', on='Timestep')
        stepthree=pd.merge(steptwo,practiceoneeco4,how='inner', on='Timestep')
        step4=pd.merge(stepthree,practiceoneeco5,how='inner', on='Timestep')
        step5=pd.merge(step4,practiceoneeco6,how='inner', on='Timestep')
        step6=pd.merge(step5,practiceoneeco7,how='inner', on='Timestep')
        step7=pd.merge(step6,practiceoneeco8,how='inner', on='Timestep')
        step8=pd.merge(step7,practiceoneeco9,how='inner', on='Timestep')
        step9=pd.merge(step8,practiceoneeco10,how='inner', on='Timestep')
        step9.columns.tolist()
        
        ### Order Columns 
        colorder=["Timestep","eco2","eco3","eco4","eco5","eco6","eco7","eco8","eco9",
                  "eco10","eco11","eco2VAR","eco3VAR","eco4VAR","eco5VAR","eco6VAR","eco7VAR",
                  "eco8VAR","eco9VAR","eco10VAR","eco11VAR",
                  "eco2STD","eco3STD","eco4STD","eco5STD","eco6STD","eco7STD","eco8STD","eco9STD",
                  "eco10STD","eco11STD"]
        ### Make it pretty 
        Print_Ready=step9[colorder]
        Print_Ready.to_csv(Drive+"Temp/"+modelname+".csv")
        os. remove(Drive+"Temp/"+modelname+".nc") 
print("Converting UAS/VAS to Windspeed and Direction")
Location=(Drive+"Temp/")        
Ecoregionnumber=len(Ecos)
MFB.CleaningWind(Modelname,Location,Ecos)
###Writing File 
print("Writing to file in outputs")
MFB.WriteitLikeLandis(Drive,Location,Ecoregionnumber,Modelname)
