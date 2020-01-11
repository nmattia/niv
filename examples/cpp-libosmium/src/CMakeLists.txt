# ---[ Include sources
set(SOURCES_CPP
	main.cpp
)

# ---[ Fetch all include headers
file(GLOB_RECURSE INCLUDE_HEADERS include/*.h)

# ---[ Add the executable
add_executable(${PROJECT_NAME} ${SOURCES_CPP} ${INCLUDE_HEADERS})

#link executable to dependency libraries
target_link_libraries (
	${PROJECT_NAME} 
	${OSMIUM_LIBRARIES}
)
