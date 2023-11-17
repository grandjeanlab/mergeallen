Merge Allen
================

This is a simple script to download the Allen Institute Brain atlas
(ccfv3) and merge smaller ROI togethers as a function of their parent
branches.

These are the packages you need to install and import before running the
script.

``` r
library(xml2)
library(nat)
```

    Loading required package: rgl

    This build of rgl does not include OpenGL functions.  Use
     rglwidget() to display results, e.g. via options(rgl.printRglwidget = TRUE).

    Registered S3 method overwritten by 'nat':
      method             from
      as.mesh3d.ashape3d rgl 

    Some nat functions depend on a CMTK installation. See ?cmtk and README.md for details.


    Attaching package: 'nat'

    The following objects are masked from 'package:base':

        intersect, setdiff, union

``` r
library(oro.nifti)
```

    oro.nifti 0.11.4


    Attaching package: 'oro.nifti'

    The following object is masked from 'package:nat':

        origin

``` r
library(abind)
```

Set the working directory to where you want to save the files.

``` r
setwd('/project/4180000.36/merge_allen')
```

Download the annotation file from the Allen Institute website. This is a
3D array with the same size as the Allen Institute Brain atlas. The
array contains the ROI ID for each voxel.

``` r
#download allen map
file.address<-"http://download.alleninstitute.org/informatics-archive/current-release/mouse_ccf/annotation/ccf_2017/annotation_25.nrrd"
download.file(file.address,'annotation_25.nrrd')
annotation<-read.nrrd('annotation_25.nrrd')
```

    Warning in readBin(fc, what = dataTypes$what[i], n = dataLength, size =
    dataTypes$size[i], : 'signed = FALSE' is only valid for integers of sizes 1 and
    2

``` r
lr.limit<-dim(annotation)[3]/2
```

Make a blank array with the same size as the annotation file. This will
be used to store the new ROI ID.

``` r
#make an empty array with the same size as the annotation
annotation.blank<-array(0,dim = dim(annotation))

#make a left and right hemisphere array
annotation.left<-array(0,dim=c(dim(annotation)[1:2],lr.limit))
annotation.right<-array(1,dim=c(dim(annotation)[1:2],lr.limit))
annotation.hemisphere<-abind(annotation.left,annotation.right,along = 3)
```

This is the most important part of the script. roi.branch is where you
list the parent roi “ID” that you want to keep. All branches from the
parents will be merged.

``` r
#list of ROIs with branches to merge
roi.branch<-c(184, 500, 353, 329, 337, 345, 369, 361, 182305689, 378, 1057, 677, 247, 669, 31, 972, 44, 714, 95, 254, 22, 541, 922, 895, 1080, 822, 703, 672, 56, 803, 549, 1097, 313, 771, 354, 512)
roi.exclude<-c()
```

Download the roi list from the Allen API and put it into a dataframe.
There is an option to exclude some ROIs if you don’t want sub-branches
to be merged with others.

``` r
xml.file<-'http://api.brain-map.org/api/v2/structure_graph_download/1.xml'


d<-read_xml(xml.file)

id <- xml_double(xml_find_all(d, ".//id"))
parent.id<-xml_double(xml_find_all(d, ".//parent-structure-id"))
```

    Warning in xml_double.xml_node(X[[i]], ...): NAs introduced by coercion

``` r
acronym<-xml_text(xml_find_all(d, ".//acronym"))
name<-xml_text(xml_find_all(d, ".//name"))
hex<-xml_text(xml_find_all(d, ".//color-hex-triplet"))
level<-xml_double(xml_find_all(d, ".//st-level"))

dd<-data.frame(id,parent.id,acronym,name,hex,level)
#dd<-dd[-c(which(dd$id %in% roi.exclude)),]
```

This is the main loop. It will go through each parent ROI and merge all
the branches together.

``` r
d.roi.branch.list<-c()

for(i in 1:length(roi.branch)){
  
roi.branch.list<-c()
  
d.id.select<-which(dd$id==roi.branch[i])
d.level.select<-dd$level[d.id.select]

print(paste('now doing ', dd$acronym[d.id.select], round(100*i/length(roi.branch),2), '% left', sep=''))

roi.branch.list<-roi.branch[i]

if(d.level.select<max(dd$level)){
for(j in (d.level.select+1): max(dd$level)){
  roi.branch.list<-unique(c(roi.branch.list,dd$id[which(dd$parent.id %in% roi.branch.list)]))
}}

d.roi.branch.list[i]<-paste(roi.branch.list,collapse = ',')

l.select<-which(annotation %in% roi.branch.list & annotation.hemisphere==0)
r.select<-which(annotation %in% roi.branch.list & annotation.hemisphere==1)
  
annotation.blank[l.select]<-i
annotation.blank[r.select]<-i+length(roi.branch)
}
```

    [1] "now doing \"FRP\"2.78% left"
    [1] "now doing \"MO\"5.56% left"
    [1] "now doing \"SSp-n\"8.33% left"
    [1] "now doing \"SSp-bfd\"11.11% left"
    [1] "now doing \"SSp-ll\"13.89% left"
    [1] "now doing \"SSp-m\"16.67% left"
    [1] "now doing \"SSp-ul\"19.44% left"
    [1] "now doing \"SSp-tr\"22.22% left"
    [1] "now doing \"SSp-un\"25% left"
    [1] "now doing \"SSs\"27.78% left"
    [1] "now doing \"GU\"30.56% left"
    [1] "now doing \"VISC\"33.33% left"
    [1] "now doing \"AUD\"36.11% left"
    [1] "now doing \"VIS\"38.89% left"
    [1] "now doing \"ACA\"41.67% left"
    [1] "now doing \"PL\"44.44% left"
    [1] "now doing \"ILA\"47.22% left"
    [1] "now doing \"ORB\"50% left"
    [1] "now doing \"AI\"52.78% left"
    [1] "now doing \"RSP\"55.56% left"
    [1] "now doing \"PTLp\"58.33% left"
    [1] "now doing \"TEa\"61.11% left"
    [1] "now doing \"PERI\"63.89% left"
    [1] "now doing \"ECT\"66.67% left"
    [1] "now doing \"HIP\"69.44% left"
    [1] "now doing \"RHP\"72.22% left"
    [1] "now doing \"CTXsp\"75% left"
    [1] "now doing \"CP\"77.78% left"
    [1] "now doing \"ACB\"80.56% left"
    [1] "now doing \"PAL\"83.33% left"
    [1] "now doing \"TH\"86.11% left"
    [1] "now doing \"HY\"88.89% left"
    [1] "now doing \"MB\"91.67% left"
    [1] "now doing \"P\"94.44% left"
    [1] "now doing \"MY\"97.22% left"
    [1] "now doing \"CB\"100% left"

``` r
dd.select<-rbind(dd[dd$id %in% roi.branch,],dd[dd$id %in% roi.branch,])
dd.select$branches<-c(rep(d.roi.branch.list,2))
dd.select$hemisphere<-c(rep('left',length(roi.branch)),rep('right',length(roi.branch)))
dd.select$new.id<-1:(length(roi.branch)*2)


write.csv2(dd.select,'ABI_atlas_reduced.csv')

nii<-nifti(annotation.blank,pixdim=c(1, 0.025,0.025,0.025,1),datatype=8)
writeNIfTI(nii,'ABI_atlas_reduce',onefile=TRUE,gzipped=TRUE)
```

    [1] "ABI_atlas_reduce.nii.gz"

``` r
#save(list=c('annotation.blank'))
```

finally, we need to swap the image dimensions using FSL and add axis
labels. I do this with AFNI.

``` bash
cd /project/4180000.36/merge_allen
fslswapdim ABI_atlas_reduce.nii.gz z -x -y swap.nii.gz
3dresample -prefix ABI_atlas_reduce_resample.nii.gz -inset swap.nii.gz
rm swap.nii.gz
```

    bash: line 2: fslswapdim: command not found
    bash: line 3: 3dresample: command not found
