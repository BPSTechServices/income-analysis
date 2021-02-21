## Variables to assist in coding
pdxpumas <- c(1314, 1301, 1305, 1303, 1302) # east county: 1316

# Which Group Quarters to include
gq_filter <- c("Households under 1970 definition", "Additional households under 1990 definition",
               "Additional households under 2000 definition")

# Educational attainment
edu_lths <- c("Nursery school to grade 4", "Nursery school, preschool", "Kindergarten", "Grade 1, 2, 3, or 4", "Grade 1", "Grade 2", "Grade 3", 
              "Grade 4", "Grade 5, 6, 7, or 8", "Grade 5 or 6", "Grade 5", "Grade 6", "Grade 7 or 8", "Grade 7", "Grade 8", "Grade 9", "Grade 10", 
              "Grade 11", "Grade 12", "12th grade, no diploma", "No schooling completed")

edu_hs <- c("High school graduate or GED", "Regular high school diploma", "GED or alternative credential")

edu_sc <- c("Some college, but less than 1 year", "1 year of college", "1 or more years of college credit, no degree", "2 years of college", 
            "Associate's degree, type not specified", "Associate's degree, occupational program", "Associate's degree, academic program", 
            "3 years of college", "4 years of college")

edu_ba <- c("Bachelor's degree", "5+ years of college", "6 years of college (6+ in 1960-1970)", "7 years of college", "8+ years of college", 
            "Master's degree", "Professional degree beyond a bachelor's degree", "Doctoral degree")



#LANGUAGE and LANGUAGED
ee_slavic_lang = c('Rumanian', 'Albanian', 'Russian', 'Russian, Great Russian', 'Bielo-, White Russian', 'Ukrainian, Ruthenian, Little Russian', 'Ruthenian', 
                   'Little Russian', 'Ukrainian', 'Czech', 'Bohemian', 'Moravian', 'Polish', 'Kashubian, Slovincian', 'Slovak', 'Serbo-Croatian, Yugoslavian, Slavonian', 
                   'Croatian', 'Serbian', 'Dalmatian', 'Montenegrin', 'Slovene', 'Lithuanian', 'Lettish, Latvian', 'Other Balto-Slavic', 'Bulgarian', 
                   'Lusatian, Sorbian, Wendish', 'Wendish', 'Macedonian', 'Slavic unknown', 'Armenian', 'Romany, Gypsy', 'Gypsy', 'Finnish', 'Magyar, Hungarian', 
                   'Magyar', 'Hungarian', 'Uralic', 'Estonian, Ingrian, Livonian, Vepsian, Votic', 'Lapp, Inari, Kola, Lule, Pite, Ruija, Skolt, Ume', 'Other Uralic', 
                   'Other Altaic', 'Chuvash', 'Karakalpak', 'Kazakh', 'Kirghiz', 'Karachay, Tatar, Balkar, Bashkir, Kumyk', 'Uzbek, Uighur', 'Azerbaijani', 'Turkmen', 
                   'Yakut', 'Caucasian, Georgian, Avar', 'Georgian', 'Lappish', 'Estonian', 'Dalmatian, Montenegrin', 'Great Russian', 'Bosnian',
                   'Rumanian', 'Albanian', 'Russian', 'Ukrainian, Ruthenian, Little Russian', 'Czech', 'Polish', 'Slovak', 'Serbo-Croatian, Yugoslavian, Slavonian', 
                   'Slovene', 'Lithuanian', 'Other Balto-Slavic', 'Slavic unknown', 'Armenian', 'Romany, Gypsy', 'Finnish', 'Magyar, Hungarian')

ee_slavic_bpl = c('Turkmenistan', 'Tadzhik', 'Kirghizia', 'Kazakhstan', 'Republic of Georgia', 'Azerbaijan', 'Armenia', 'Ukraine', 'Bessarabia', 
                  'Moldavia', 'Byelorussia', 'Other USSR/Russia', 'Baltic States, ns', 'Lithuania', 'Latvia', 'Estonia', 'Eastern Europe, ns', 
                  'Central Europe, ns', 'Kosovo', 'Slovenia', 'Carniola', 'Slovonia', 'Dalmatia', 'Bosnia', 'Serbia', 'Montenegro', 'Croatia', 
                  'Yugoslavia', 'Transylvania', 'Romania', 'Russian Poland', 'West Prussia', 'Silesia', 'Prussian Poland', 'Posen', 'Pomerania', 
                  'East Prussia', 'German Poland', 'Galicia', 'Austrian Poland', 'Poland', 'Hungary', 'Finland', 'Lapland, ns', 'Svalbard and Jan Meyen', 
                  'Svalbard', 'Albania', 'Bulgaria', 'Czechoslovakia', 'Bohemia', 'Bohemia-Moravia', 'Slovakia', 'Czech Republic', 'East Berlin', 
                  'East Germany', 'USSR, ns', 'Siberia', 'Uzbekistan')

ee_slavic_ancest = c('Armenian', 'Eastern European, nec', 'Central European, nec', 'Slavonian', 'Slav', 'Yugoslavian', 'Windish', 'Husel', 'Bioko', 
                     'Lemko', 'Ruthenian (1990-2000)', 'Ruthenian (1980)', 'Ukrainian (1990-2000, ACS, PRCS)', 'Ukrainian (1980)', 'Uzbek', 
                     'Tadzhik (1980, 2000)', 'Soviet Central Asia (1990-2000)', 'Tuvinian (1990-2000)', 'Crimean (1980)', 'Tartar (1980)', 
                     'Tatar (1990-2000)', 'Soviet Union, nec', 'Yakut', 'Mesknetian (1990-2000)', 'Gagauz (1990-2000)', 'Chevash', 'Bashkir', 
                     'Soviet Turkic (1990-2000)', 'Sorb/Wend', 'Slovene', 'Slovak', 'Montenegrin (1990-2000, 2012 ACS)', 
                     'Bosnian (1990) Herzegovinian (2000, ACS, PRCS)', 'Serbian (1990-2000, ACS, PRCS)', 'Serbian (1980)', 'Muscovite', 'Russian', 
                     'Wallachian', 'Moldavian', 'Bucovina', 'Bessarabian (1990-2000)', 'Bessarabian (1980)', 'Transylvanian', 'Rumanian (1980)', 
                     'Romanian (1990-2000, ACS, PRCS)', 'Kashubian', 'Polish', 'Ossetian', 'North Caucasian Turkic (1990-2000)', 'North Caucasian', 
                     'Macedonian', 'Lithuanian', 'Latvian', 'Magyar', 'Hungarian', 'Rom', 'Gruziia (1990-2000)', 
                     'German from Russia (1990-2000); German Russian (ACS, PRCS)', 'Volga', 'Germans from Russia', 'Georgian', 'Voytak', 'Mordovian', 
                     'Udmert', 'Finno Ugrian (1990-2000)', 'Livonian', 'Estonian', 'Moravian (1990-2000)', 'Bohemian (1990-2000, ACS, PRCS)', 'Bohemian', 
                     'Czech', 'Czechoslovakian', 'Croatian', 'Turcoman (1980)', 'Kirghiz (1980)', 'Turkestani (1990-2000, 2012 ACS)', 'Cossack (1980)', 
                     'Cossack (1990-2000)', 'Rusyn', 'Carpatho Rusyn', 'Carpathian', 'Bulgarian', 'Belorussian', 'Azerbaijani', 'Albanian', 'Silesian (1990-2000)', 
                     'East German (1990-2000)', 'Finnish')

