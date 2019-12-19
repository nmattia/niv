# ---[ LibOsmium
find_package(Osmium REQUIRED COMPONENTS pbf)
include_directories(SYSTEM ${OSMIUM_INCLUDE_DIRS})
