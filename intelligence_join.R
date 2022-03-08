library(tidyr)
library(lubridate)

dogs <- animals %>%
  filter(Animal_Type == 'Dog') %>%
  select(-c('Found Location',
            'Name',
            'Animal_Type')) %>%
  rename(Outcome_Type = 'Outcome Type') %>%
  filter(!is.na(Outcome_DateTime))

# Adding a binary indicator of whether of not the dog is mixed breed:
dogs$Mix <- ifelse(grepl('Mix', dogs$Breed) | grepl('/', dogs$Breed), 1, 0)
# table(dogs$Mix)

# Remove 'Mix' from breed title; data is captured by flag indicator:
dogs$Breed <- gsub(' Mix', '', dogs$Breed) 

# Creates second column for additional breed if provided:
dogs <- dogs %>%
  separate(Breed, c('Breed', 'Second_Breed'), sep = '/', extra = 'drop', fill = 'right') 

#Fixing Breed naming to match AKC intelligence data:
dogs$Breed <- gsub('Alaskan Husky', 'Siberian Husky', dogs$Breed) 
dogs$Second_Breed <- gsub('Alaskan Husky', 'Siberian Husky', dogs$Second_Breed)

dogs$Breed <- gsub('American Bulldog', 'Bulldog', dogs$Breed) 
dogs$Second_Breed <- gsub('American Bulldog', 'Bulldog', dogs$Second_Breed)

dogs$Breed <- gsub('Belgian Tervuren', 'Belgian Shepherd Dog (Tervuren)', dogs$Breed) 
dogs$Second_Breed <- gsub('Belgian Tervuren', 'Belgian Shepherd Dog (Tervuren)', dogs$Second_Breed)

dogs$Breed <- gsub('Bouv Flandres', 'Bouvier des Flandres', dogs$Breed) 
dogs$Second_Breed <- gsub('Bouv Flandres', 'Bouvier des Flandres', dogs$Second_Breed)

dogs$Breed <- gsub('Bruss Griffon', 'Griffon Bruxellois', dogs$Breed) 
dogs$Second_Breed <- gsub('Bruss Griffon', 'Griffon Bruxellois', dogs$Second_Breed)

dogs$Breed <- gsub('Bull Terrier Miniature', 'Bull Terrior', dogs$Breed) 
dogs$Second_Breed <- gsub('Bull Terrier Miniature', 'Bull Terrior', dogs$Second_Breed)

dogs$Breed <- gsub('Chinese Sharpei', 'Chinese Shar Pei', dogs$Breed) 
dogs$Second_Breed <- gsub('Chinese Sharpei', 'Chinese Shar Pei', dogs$Second_Breed)

dogs$Breed <- gsub('Chihuahua Shorthair', 'Chihuahua', dogs$Breed) 
dogs$Second_Breed <- gsub('Chihuahua Shorthair', 'Chihuahua', dogs$Second_Breed)

dogs$Breed <- gsub('Chihuahua Longhair', 'Chihuahua', dogs$Breed) 
dogs$Second_Breed <- gsub('Chihuahua Longhair', 'Chihuahua', dogs$Second_Breed)

dogs$Breed <- gsub('Dachshund Longhair', 'Dachshund', dogs$Breed) 
dogs$Second_Breed <- gsub('Dachshund Longhair', 'Dachshund', dogs$Second_Breed)

dogs$Breed <- gsub('Dachshund Wirehair', 'Dachshund', dogs$Breed) 
dogs$Second_Breed <- gsub('Dachshund Wirehair', 'Dachshund', dogs$Second_Breed)

dogs$Breed <- gsub('Dachshund Stan', 'Dachshund', dogs$Breed) 
dogs$Second_Breed <- gsub('Dachshund Stan', 'Dachshund', dogs$Second_Breed)

dogs$Breed <- gsub('Doberman Pinsch', 'Doberman Pinscher', dogs$Breed) 
dogs$Second_Breed <- gsub('Doberman Pinsch', 'Doberman Pinscher', dogs$Second_Breed)

dogs$Breed <- gsub('Wire Hair Fox Terrier', 'Wire Fox Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Wire Hair Fox Terrier', 'Wire Fox Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Bull Terrier Miniature', 'Bull Terrior', dogs$Breed) 
dogs$Second_Breed <- gsub('Alaskan Husky', 'Siberian Husky', dogs$Second_Breed)

dogs$Breed <- gsub('Toy Poodle', 'Poodle', dogs$Breed) 
dogs$Second_Breed <- gsub('Toy Poodle', 'Poodle', dogs$Second_Breed)

dogs$Breed <- gsub('Soft Coated Wheaten Terrier', 'Soft-coated Wheaten Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Soft Coated Wheaten Terrier', 'Soft-coated Wheaten Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Smooth Fox Terrier', 'Fox Terrier (Smooth)', dogs$Breed) 
dogs$Second_Breed <- gsub('Smooth Fox Terrier', 'Fox Terrier (Smooth)', dogs$Second_Breed)

dogs$Breed <- gsub('Eng Toy Spaniel', 'English Cocker Spaniel', dogs$Breed) 
dogs$Second_Breed <- gsub('Eng Toy Spaniel', 'English Cocker Spaniel', dogs$Second_Breed)

dogs$Breed <- gsub('English Bulldog', 'Bulldog', dogs$Breed) 
dogs$Second_Breed <- gsub('English Bulldog', 'Bulldog', dogs$Second_Breed)

dogs$Breed <- gsub('German Shorthair Pointer', 'German Shorthaired Pointer', dogs$Breed) 
dogs$Second_Breed <- gsub('German Shorthair Pointer', 'German Shorthaired Pointer', dogs$Second_Breed)

dogs$Breed <- gsub('Jack Russell Terrier', 'Jack Russell terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Jack Russell Terrier', 'Jack Russell terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Port Water Dog', 'Portuguese Water Dog', dogs$Breed) 
dogs$Second_Breed <- gsub('Port Water Dog', 'Portuguese Water Dog', dogs$Second_Breed)

dogs$Breed <- gsub('Schnauzer Giant', 'Giant Schnauzer', dogs$Breed) 
dogs$Second_Breed <- gsub('Schnauzer Giant', 'Giant Schnauzer', dogs$Second_Breed)

dogs$Breed <- gsub('Spanish Mastiff', 'Mastiff', dogs$Breed) 
dogs$Second_Breed <- gsub('Spanish Mastiff', 'Mastiff', dogs$Second_Breed)

dogs$Breed <- gsub('St. Bernard Rough Coat', 'Saint Bernard', dogs$Breed) 
dogs$Second_Breed <- gsub('St. Bernard Rough Coat', 'Saint Bernard', dogs$Second_Breed)

dogs$Breed <- gsub('St. Bernard Smooth Coat', 'Saint Bernard', dogs$Breed) 
dogs$Second_Breed <- gsub('St. Bernard Smooth Coat', 'Saint Bernard', dogs$Second_Breed)

dogs$Breed <- gsub('Standard Poodle', 'Poodle', dogs$Breed) 
dogs$Second_Breed <- gsub('Standard Poodle', 'Poodle', dogs$Second_Breed)

dogs$Breed <- gsub('Bedlington Terr', 'Bedlington Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Bedlington Terr', 'Bedlington Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Cavalier Span', 'Cavalier King Charles Spaniel', dogs$Breed) 
dogs$Second_Breed <- gsub('Cavalier Span', 'Cavalier King Charles Spaniel', dogs$Second_Breed)

dogs$Breed <- gsub('English Coonhound', 'English Foxhound', dogs$Breed) 
dogs$Second_Breed <- gsub('English Coonhound', 'English Foxhound', dogs$Second_Breed)

dogs$Breed <- gsub('Flat Coat Retriever', 'Flat-Coated Retriever', dogs$Breed) 
dogs$Second_Breed <- gsub('Flat Coat Retriever', 'Flat-Coated Retriever', dogs$Second_Breed)

dogs$Breed <- gsub('Rhod Ridgeback', 'Rhodesian Ridgeback', dogs$Breed) 
dogs$Second_Breed <- gsub('Rhod Ridgeback', 'Rhodesian Ridgeback', dogs$Second_Breed)

dogs$Breed <- gsub('Miniature Poodle', 'Poodle', dogs$Breed) 
dogs$Second_Breed <- gsub('Miniature Poodle', 'Poodle', dogs$Second_Breed)

dogs$Breed <- gsub('Treeing Walker Coonhound', 'American Foxhound', dogs$Breed) 
dogs$Second_Breed <- gsub('Treeing Walker Coonhound', 'American Foxhound', dogs$Second_Breed)

dogs$Breed <- gsub('Queensland Heeler', 'Australian Cattle Dog', dogs$Breed) 
dogs$Second_Breed <- gsub('Queensland Heeler', 'Australian Cattle Dog', dogs$Second_Breed)

dogs$Breed <- gsub('Bluetick Hound', 'American Foxhound', dogs$Breed) 
dogs$Second_Breed <- gsub('Bluetick Hound', 'American Foxhound', dogs$Second_Breed)

dogs$Breed <- gsub('Bull Terrior', 'Bull Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Bull Terrior', 'Bull Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Collie Rough', 'Collie', dogs$Breed) 
dogs$Second_Breed <- gsub('Collie Rough', 'Collie', dogs$Second_Breed)

dogs$Breed <- gsub('Collie Smooth', 'Collie', dogs$Breed) 
dogs$Second_Breed <- gsub('Collie Smooth', 'Collie', dogs$Second_Breed)

dogs$Breed <- gsub('Dandie Dinmont', 'Dandie Dinmont Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Dandie Dinmont', 'Dandie Dinmont Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Dutch Sheepdog', 'Old English Sheepdog', dogs$Breed) 
dogs$Second_Breed <- gsub('Dutch Sheepdog', 'Old English Sheepdog', dogs$Second_Breed)

dogs$Breed <- gsub('German Pinscher', 'Affenpinscher', dogs$Breed) 
dogs$Second_Breed <- gsub('German Pinscher', 'Affenpinscher', dogs$Second_Breed)

dogs$Breed <- gsub('Mastiff', 'Neapolitan Mastiff', dogs$Breed) 
dogs$Second_Breed <- gsub('Mastiff', 'Neapolitan Mastiff', dogs$Second_Breed)

dogs$Breed <- gsub('Parson Russell Terrier', 'Jack Russell terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Parson Russell Terrier', 'Jack Russell terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Picardy Sheepdog', 'Old English Sheepdog', dogs$Breed) 
dogs$Second_Breed <- gsub('Picardy Sheepdog', 'Old English Sheepdog', dogs$Second_Breed)

dogs$Breed <- gsub('Redbone Hound', 'American Foxhound', dogs$Breed) 
dogs$Second_Breed <- gsub('Redbone Hound', 'AMerican Foxhound', dogs$Second_Breed)

dogs$Breed <- gsub('Spanish Water Dog', 'Portuguese Water Dog', dogs$Breed) 
dogs$Second_Breed <- gsub('Spanish Water Dog', 'Portuguese Water Dog', dogs$Second_Breed)

dogs$Breed <- gsub('Tibetan Mastiff', 'Mastiff', dogs$Breed) 
dogs$Second_Breed <- gsub('Tibetan Mastiff', 'Mastiff', dogs$Second_Breed)

dogs$Breed <- gsub('Wirehaired Vizsla', 'Vizsla', dogs$Breed) 
dogs$Second_Breed <- gsub('Wirehaired Vizsla', 'Vizsla', dogs$Second_Breed)

dogs$Breed <- gsub('Alaskan Klee Kai', 'Alaskan Malamute', dogs$Breed) 
dogs$Second_Breed <- gsub('Alaskan Klee Kai', 'Alaskan Malamute', dogs$Second_Breed)

dogs$Breed <- gsub('Boerboel', 'Mastiff', dogs$Breed) 
dogs$Second_Breed <- gsub('Boerboel', 'Mastiff', dogs$Second_Breed)

dogs$Breed <- gsub('Glen Of Imaal', 'Irish Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Glen Of Imaal', 'Irish Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Harrier', 'English Foxhound', dogs$Breed) 
dogs$Second_Breed <- gsub('Harrier', 'English Foxhound', dogs$Second_Breed)

dogs$Breed <- gsub('Sealyham Terr', 'Sealyham Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Sealyham Terr', 'Sealyham Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Toy Fox Terrier', 'Fox Terrier (Smooth)', dogs$Breed) 
dogs$Second_Breed <- gsub('Toy Fox Terrier', 'Fox Terrier (Smooth)', dogs$Second_Breed)

dogs$Breed <- gsub('Chesa Bay Retr', 'Chesapeake Bay Retriever', dogs$Breed) 
dogs$Second_Breed <- gsub('Chesa Bay Retr', 'Chesapeake Bay Retriever', dogs$Second_Breed)

dogs$Breed <- gsub('English Pointer', 'Pointer', dogs$Breed) 
dogs$Second_Breed <- gsub('English Pointer', 'Pointer', dogs$Second_Breed)

dogs$Breed <- gsub('Silky Terrier', 'Australian Silky Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Silky Terrier', 'Australian Silky Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Tibetan Neapolitan Mastiff', 'Mastiff', dogs$Breed) 
dogs$Second_Breed <- gsub('Tibetan Neapolitan Mastiff', 'Mastiff', dogs$Second_Breed)

dogs$Breed <- gsub('Grand Basset Griffon Vendeen', 'Grand Basset Griffon Venden', dogs$Breed) 
dogs$Second_Breed <- gsub('Grand Basset Griffon Vendeen', 'Grand Basset Griffon Venden', dogs$Second_Breed)

dogs$Breed <- gsub('Neapolitan Mastiff', 'Mastiff', dogs$Breed) 
dogs$Second_Breed <- gsub('Neapolitan Mastiff', 'Mastiff', dogs$Second_Breed)

dogs$Breed <- gsub('Old Bulldog', 'Bulldog', dogs$Breed) 
dogs$Second_Breed <- gsub('Old Bulldog', 'Bulldog', dogs$Second_Breed)

dogs$Breed <- gsub('West Highland', 'West Highland White Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('West Highland', 'West Highland White Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Dutch Shepherd', 'German Shepherd', dogs$Breed) 
dogs$Second_Breed <- gsub('Dutch Shepherd', 'German Shepherd', dogs$Second_Breed)

dogs$Breed <- gsub('Rat Terrier', 'Fox Terrier (Smooth)', dogs$Breed) 
dogs$Second_Breed <- gsub('Rat Terrier', 'Fox Terrier (Smooth)', dogs$Second_Breed)

dogs$Breed <- gsub('Staffordshire', 'American Staffordshire Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Staffordshire', 'American Staffordshire Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('American Pit Bull Terrier', 'American Staffordshire Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('American Pit Bull Terrier', 'American Staffordshire Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Pit Bull', 'American Staffordshire Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('Pit Bull', 'American Staffordshire Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('American American Staffordshire Terrier Terrier', 'American Staffordshire Terrier', dogs$Breed) 
dogs$Second_Breed <- gsub('American American Staffordshire Terrier Terrier', 'American Staffordshire Terrier', dogs$Second_Breed)

dogs$Breed <- gsub('Black', 'American Foxhound', dogs$Breed) 
dogs$Second_Breed <- gsub('Tan Hound', '', dogs$Second_Breed)

dogs$Breed <- gsub('American Foxhound Mouth Cur', 'Black Mouth Cur', dogs$Breed) 

# Merging dogs, intelligence tables:
intelligence$Breed <- as.character(intelligence$Breed)

dogs_int <- dogs %>%
  left_join(intelligence, by = c('Breed' = 'Breed'))

# Exploring missing matches:
missing_int <- dogs_int %>%
  filter(is.na(Classification))
table(missing_int$Breed) # Successfully fixed all but 6,611!
remove(missing_int)
