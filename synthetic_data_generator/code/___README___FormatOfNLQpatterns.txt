# FORMAT IN NLQ pattern files

# attrName = "Gender"
# attrPattern = "of_$Gender_students" -> "of gender", "in_$Language" -> "in language" 
# return the NL version of the pattern attrPattern if it contains a match with the attribute attrName
# "of_$Type_of_pollution_pollution" c("Type_of_pollution", "Gender", "Year") -> "of Type of pollution"
# "of__$Subtype__$Food_type" & attrName = "Subtype"    => "of Subtype"
# "of__$Subtype__$Food_type" & attrName = "Food_type"  => "of Food type"
# "of_$Subtype__$Food_type"  & attrName = "Subtype"    => "of Subtype"
# "of_$Subtype__$Food_type"  & attrName = "Food_type"  => "Food type"
# "of_$Subtype__with_$Food_type"    & attrName = "Food_type"   => "with Food type"
# "of__$Subtype_of_food__$Food_type" & attrName = "Subtype"   => "of Subtype"
# "of__$Subtype_of_food__$Food_type" & attrName = "Food_type"   => "of Food type"
# "in__$Quarter==of==$Year"          & attrName = "Year"    => "in Year"
# "in__$Quarter==of==$Year"          & attrName = "Quarter" => "in Quarter"

# in__$Quarter==of==$Year => in Q1 of 2023 / in Q1 / in 2023  
# in__$City==,==$State => in Malibu, California / in Malibu / in California  
# of__$Subtype==$Food_type => of Cow Meat / of Cow / of Meat

# of_((pollution)) of_$Type_of_pollution_pollution of_$Polluted_element  vs  
# of_((pollution)) of_$Type_of_pollution_pollution__of_$Polluted_element


###### PARAMETERS IN TABLE GENERATOR JSON

# sample 
# [0] to pick all existing values without sampling
# [x,y] to pick between a number x and a number y of consecutive values in the list of values
# for instance: in [1,2,3,4,5] pick [2,4] can generate at random either: [1,2],[2,3],[3,4],[4,5],[1,2,3],[2,3,4],[3,4,5],[1,2,3,4],[2,3,4,5]
# for hierarchical attributes: [x,y] is applied on each level independently





BUGS
"NAME: Number_of_building_construction_set128_4 Generate DB table: 686/900 -- Replica: 4/5"
[1] "Table design is unique, proceed..."
[1] "Generate CSV for DB TABLE"
[1] "Generate HTML for DB TABLE"
[1] "Generate JSON signature (no aggregation info)"
[1] "Generate PIVOT TABLE"
[1] "************** SAVING TABLE ************* 2051"
[1] "Generate HTML for PIVOT TABLE"
[1] "Generate PDF of PIVOT TABLE"
Error in force(expr) : Failed to generate output in 30 seconds (timeout).
> 

--- replace months by short cut 3 letters
--- reduce combinations in building_constrution tables

---- accidents, put Years then quarters, not the opposite