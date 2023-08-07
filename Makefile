# This Makefile requires Gnu Make

#include configure created makefile
-include Makefile.config

# This is the project version
# It will be used in setting the version information in library init files
# as well as used in the name of generated libraries
VERSION = 0.5

# Build Tools
BIGLOO = bigloo
CC := $(shell $(BIGLOO) -eval "(print (bigloo-config 'c-compiler)) (exit 0)" -q)
JAVAC = javac
MAVEN = mvn
BGLAFILE = bglafile
BGLJFILE = bgljfile
BGLTAGS = bgltags
AFILE = .afile
JFILE = .jfile
AR = ar
ARFLAGS = qc
JAR = jar
JARFLAGS = -cf
RANLIB = ranlib
INSTALL = install
RM = rm -rf
SED = sed

BIGLOO_VERSION = $(shell $(BIGLOO) -eval "(print (bigloo-config 'release-number)) (exit 0)" -q)

BIGLOO_LIB_DIR = $(shell $(BIGLOO) -eval "(print (bigloo-config 'library-directory)) (exit 0)" -q)

PROJECT_DIR = $(shell pwd)

# The default install prefix and installation paths
INSTALL_BIN_DIR = $(INSTALL_PREFIX)/bin
INSTALL_LIB_DIR = $(INSTALL_PREFIX)/lib/bigloo/$(BIGLOO_VERSION)

# The destination for the resulting binaries and libraries. Binaries are placed
# BUILD_BIN_DIR, and libraries are placed in BUILD_LIB_DIR.
BUILD_DIR = build
BUILD_LIB_DIR = $(BUILD_DIR)/lib
BUILD_BIN_DIR = $(BUILD_DIR)/bin

# The location of project source code. Sources for binaries are found in
# SRC_BIN_DIR, and libraries are placed in SRC_LIB_DIR 
SRC_DIR = src
BIN_NAME = bin
LIB_NAME = lib
SRC_BIN_DIR = $(SRC_DIR)/$(BIN_NAME)
SRC_LIB_DIR = $(SRC_DIR)/$(LIB_NAME)
TEST_DIR = test
TEST_BUILD_DIR = $(TEST_DIR)/$(BUILD_DIR)

# C and Java Foreign Function code is found relative to binary and library
# directories in FFI_C_SRC_DIR and FFI_JAVA_SRC_DIR
FFI_C_SRC_DIR = Clib
FFI_JAVA_SRC_DIR = Jlib

# heap source file name
HEAP_SRC_BASE = make_lib
LIB_INIT_TEMPLATE = lib.init.in

# The file extensions used for libraries
STATIC_LIBRARY_EXT = a
SHARED_LIBRARY_EXT := $(shell $(BIGLOO) -eval "(print (bigloo-config 'shared-lib-suffix)) (exit 0)" -q)
ZIP_EXT = zip

# We can build safe, unsafe, and profile versions of all the
# targets. These are the suffixes we use to distinguish each variant.
VARIANT_SUFFIXES:=s u p

# BIGLOO compiler flags
BFLAGS_COMMON = -O6 -q -afile $(AFILE) -mkaddlib -fsharing -freturn-goto -fstackable -jvm-catch-errors   
BFLAGS_u = $(BFLAGS_COMMON) -unsafe -cgen
BFLAGS_s = $(BFLAGS_COMMON)
BFLAGS_p = $(BFLAGS_COMMON) -p

# Add directories to the Bigloo library search path
# By default, we add the project build lib directory to the library search path
BLFLAGS_COMMON = -L $(BUILD_LIB_DIR)
BLFLAGS_u = $(BLFLAGS_COMMON)
BLFLAGS_s = $(BLFLAGS_COMMON)
BLFLAGS_p = $(BLFLAGS_COMMON)

#Add BIGLOO libraries (e.g., -lpthread)
BLDFLAGS_COMMON = -lz -lzstd -llzma -lbz2
BLDFLAGS_u = $(BLDFLAGS_COMMON)
BLDFLAGS_s = $(BLDFLAGS_COMMON)
BLDFLAGS_p = $(BLDFLAGS_COMMON)

# Add cflags to bigloo via -copt 
BCFLAGS_COMMON = 
BCFLAGS_u = $(BCFLAGS_COMMON)
BCFLAGS_s = $(BCFLAGS_COMMON)
BCFLAGS_p = $(BCFLAGS_COMMON)

#Add Bigloo include directories to CFLAGS
CFLAGS += -I $(BIGLOO_LIB_DIR) -fPIC

# Additional java libraries should be added here
CLASSPATH_COMMON = $(BUILD_LIB_DIR)/zstd-jni-1.5.5-4.jar:$(BUILD_LIB_DIR)/xz-1.9.jar:$(BUILD_LIB_DIR)/bzip2-0.9.1.jar
CLASSPATH_u = $(CLASSPATH_COMMON):$(BIGLOO_LIB_DIR)/bigloo_u.zip
CLASSPATH_s = $(CLASSPATH_COMMON):$(BIGLOO_LIB_DIR)/bigloo_s.zip
CLASSPATH_p = $(CLASSPATH_COMMON):$(BIGLOO_LIB_DIR)/bigloo_p.zip

JVMOPTFLAGS = -Xss4M

# The bigloo compiler flags used to build the library heap files
HEAPFLAGS = -I $(SRC_LIB_DIR) -unsafe -q -mkaddheap -mkaddlib \
	     -heap-library

# We now decide what targets to include in the default release build.
# We include/exclude targets based on the supported backends.
RELEASE_TARGETS=

ifeq ($(SUPPORT_NATIVE),true) 
	RELEASE_TARGETS += release_libs release_bins
endif
ifeq ($(SUPPORT_JVM),true)
	RELEASE_TARGETS += release_zips release_jbins
endif

TESTING_TARGETS=
ifeq ($(SUPPORT_NATIVE),true) 
	TESTING_TARGETS += test_bins
endif
ifeq ($(SUPPORT_JVM),true)
	TESTING_TARGETS += test_jbins
endif

# utility functions for finding files and directories

# return the directories contained in the passed dir
define list_dirs
  $(shell if [ -d $1 ]; then cd $1 && ls -d *; fi)
endef

# recursively find files in the provided directory ($1) matching
# the given pattern ($2)
define find_files 
  $(shell if [ -d $1 ]; then \
	     find $1 -type f -name $2  ; \
          fi;)
endef

# recursively find sub-directories of the provided directory ($1)
# containing files matching the given pattern ($2). The patterns are
# those supported by find 
define find_subdirs_containing
  $(shell if [ -d $1 ]; then \
	     find $1 -name $2 -printf '%h\n'; \
          fi;)
endef

# recursively find all sub-directories of the provided directory ($1)
define find_all_subdirs
  $(shell find $1 -type d -print)
endef

# generate scm dependency information for all scm source files
# in the project
define generate_scm_dependencies
  $(shell cd $1; bgldepend -fno-mco `find -name '*.scm'  -printf '%P\n'`)
endef


# The build targets depend on the directory structure of the project.
# The directories found in src/bin and src/lib are taken to be the binary and
# library targets respectively. For example, the directory src/bin/example and the
# source code under it is interpreted as the binary target example and its source
# dependencies. Likewise, src/lib/doit is interpreted as the library target doit
# and its source dependencies. The sub-directories Clib and Jlib are treated
# specially. If they exist they are treated as foreign-function code to be
# included in the library or binary.

# Note, if we are cleaning the project, there is no reasons to perform
# this relatively costly logic, so we skip it.
ifeq (,$(filter clean clean-%,$(MAKECMDGOALS)))
  # determine our binary and library targets
  BIN_TARGETS := $(call list_dirs,$(SRC_BIN_DIR))
  LIB_TARGETS := $(call list_dirs,$(SRC_LIB_DIR))
  TEST_TARGETS := $(filter-out $(BUILD_DIR),$(call list_dirs,$(TEST_DIR)))

  # determine the objects and scheme and c sources required for each library target
  define lib_target_OBJECTS
  #LIB_$1_SCM_SOURCES_$3 := $$(filter-out $(SRC_LIB_DIR)/$1/$(HEAP_SRC_BASE).scm,$$(call find_files,$(SRC_LIB_DIR)/$1,'*.scm'))
  LIB_$1_SCM_SOURCES_$3 := $$(call find_files,$(SRC_LIB_DIR)/$1,'*.scm')
  LIB_$1_C_SOURCES_$3 := $$(call find_files,$(SRC_LIB_DIR)/$1,'*.c')
  LIB_$1_C_INCLUDE_DIRS := $$(call find_subdirs_containing,$(SRC_LIB_DIR)/$1,'*.h')
  LIB_$1_OBJECTS_$3 := $$(LIB_$1_SCM_SOURCES_$3:$(SRC_DIR)/%.scm=$2/%.o) $$(LIB_$1_C_SOURCES_$3:$(SRC_DIR)/%.c=$2/%.o)
  LIB_$1_HEAP_OBJECTS_$3 := $2/lib/$1/$(HEAP_SRC_BASE).o 
  endef

  # determine the classes and scheme and java sources required for each zip target
  define zip_target_CLASSES
  ZIP_$1_JAVA_SOURCES_$3 := $$(call find_files,$(SRC_LIB_DIR)/$1/$(FFI_JAVA_SRC_DIR),'*.java')
  ZIP_$1_JAVA_CLASSES_$3 := $$(ZIP_$1_JAVA_SOURCES_$3:$(SRC_LIB_DIR)/$1/$(FFI_JAVA_SRC_DIR)/%.java=$2/%.class)
  ZIP_$1_CLASSES_$3 := $$(LIB_$1_SCM_SOURCES_$3:$(SRC_DIR)/%.scm=$2/bigloo/%.class) $$(ZIP_$1_JAVA_CLASSES$3)
  ZIP_$1_HEAP_CLASSES_$3 := $2/bigloo/lib/$1/$(HEAP_SRC_BASE).class
  endef

  # determine the objects and scheme and c sources required for each binary target
  define bin_target_OBJECTS
  $1_SCM_SOURCES_$3 := $$(call find_files,$(SRC_BIN_DIR)/$1, '*.scm')
  $1_C_SOURCES_$3 := $$(call find_files,$(SRC_BIN_DIR)/$1/$(FFI_C_SRC_DIR),'*.c')
  $1_C_INCLUDE_DIRS := $$(call find_subdirs_containing,$(SRC_BIN_DIR)/$1,'*.h')
  $1_OBJECTS_$3 := $$($1_SCM_SOURCES_$3:$(SRC_DIR)/%.scm=$2/%.o) $$($1_C_SOURCES_$3:$(SRC_DIR)/%.c=$2/%.o)
  endef

  # determine the objects and scheme sources required for each test target
  define test_target_OBJECTS
  TEST_$1_SCM_SOURCES := $$(call find_files,$(TEST_DIR)/$1,'*.scm')
  TEST_$1_OBJECTS := $$(TEST_$1_SCM_SOURCES:$(TEST_DIR)/%.scm=$2/%.o) $$(TEST_$1_C_SOURCES:$(TEST_DIR)/%.c=$2/%.o)
  endef

  # determine the classes and scheme and java sources required for each java bin target
  define jbin_target_CLASSES
  $1_JAVA_SOURCES_$3 := $$(call find_files,$(SRC_BIN_DIR)/$1/$(FFI_JAVA_SRC_DIR),'*.java')
  $1_JAVA_CLASSES_$3 := $$($1_JAVA_SOURCES_$3:$(SRC_BIN_DIR)/$1/$(FFI_JAVA_SRC_DIR)/%.java=$2/%.class)
  $1_CLASSES_$3 := $$($1_SCM_SOURCES_$3:$(SRC_DIR)/%.scm=$2/bigloo/%.class) $$($1_JAVA_CLASSES_$3)

  # we create the classpath for the java bin. It includes both the build artifact paths
  # as well as the installation paths to assure it can be ran from both locations.
  # TODO: find a cleaner way to do this
  $1_CLASSPATH_$3 := "\`dirname \$$$$0\`:\`dirname \$$$$0\`/j$1_$3.jar:$(PROJECT_DIR)/$(BUILD_LIB_DIR)/j$1_$3.jar$(LIB_TARGETS:%=:$(PROJECT_DIR)/$(BUILD_LIB_DIR)/%_$3-$(VERSION).zip)$(LIB_TARGETS:%=:$(INSTALL_LIB_DIR)/%_$3-$(VERSION).zip):$(CLASSPATH_$3)"
  endef

  # determine the clases needed for each test target
  define test_target_CLASSES
  TEST_$1_CLASSES := $$(TEST_$1_SCM_SOURCES:$(TEST_DIR)/%.scm=$2/bigloo/test/%.class)
  TEST_$1_CLASSPATH := "\`dirname \$$$$0\`:\`dirname \$$$$0\`/j$1.jar:$(PROJECT_DIR)/$(BUILD_LIB_DIR)/j$1.jar$(LIB_TARGETS:%=:$(PROJECT_DIR)/$(BUILD_LIB_DIR)/%_s-$(VERSION).zip):$(CLASSPATH_$3)"
  endef

  # add executable and library OBJECTS variables
  $(foreach T, $(BIN_TARGETS),$(foreach V,$(VARIANT_SUFFIXES),$(eval $(call bin_target_OBJECTS,$T,.Olib_$V,$V))))
  $(foreach T, $(LIB_TARGETS),$(foreach V,$(VARIANT_SUFFIXES),$(eval $(call lib_target_OBJECTS,$T,.Olib_$V,$V))))
  $(foreach T, $(LIB_TARGETS),$(foreach V,$(VARIANT_SUFFIXES),$(eval $(call zip_target_CLASSES,$T,.class_$V,$V))))
  $(foreach T, $(BIN_TARGETS),$(foreach V,$(VARIANT_SUFFIXES),$(eval $(call jbin_target_CLASSES,$T,.class_$V,$V))))
  $(foreach T, $(TEST_TARGETS), $(eval $(call test_target_OBJECTS,$T,.Olib_s)))
  $(foreach T, $(TEST_TARGETS), $(eval $(call test_target_CLASSES,$T,.class_s)))

  # add dependencies 
  $(eval $(call generate_scm_dependencies,$(SRC_DIR)))

  # determine the include directories and flags required for binary targets
  define bin_TARGET_INCLUDE_FLAGS
  $1_INCLUDE_DIRS := $$(call find_all_subdirs,$(SRC_BIN_DIR)/$1)
  $1_INCLUDE_FLAGS := $$($1_INCLUDE_DIRS:%=-I %)
  endef

  # add the include dirs and flag variables for each binary target
  $(foreach T, $(BIN_TARGETS), $(eval $(call bin_TARGET_INCLUDE_FLAGS,$T)))

  # determine the include directories and flags required for each library target
  define lib_TARGET_INCLUDE_FLAGS
  LIB_$1_INCLUDE_DIRS := $$(call find_all_subdirs,$(SRC_LIB_DIR)/$1)
  LIB_$1_INCLUDE_FLAGS := $$(LIB_$1_INCLUDE_DIRS:%=-I %)
  endef

   # add the include dirs and flag variables for each library target
  $(foreach T,$(LIB_TARGETS),$(eval $(call lib_TARGET_INCLUDE_FLAGS,$T)))

  # determine the include directories and flags required for binary targets 
  define test_TARGET_INCLUDE_FLAGS
  TEST_$1_INCLUDE_DIRS := $$(call find_all_subdirs,$(TEST_DIR)/$1)
  TEST_$1_INCLUDE_FLAGS := $$(TEST_$1_INCLUDE_DIRS:%=-I %)
  endef

  # add the include dirs and flag variables for each test target
  $(foreach T, $(TEST_TARGETS), $(eval $(call test_TARGET_INCLUDE_FLAGS,$T)))
endif

# Suffixes
.SUFFIXES: 
.SUFFIXES: .scm .o .c .java


.PHONY: clean all release release_bins release_libs release_zips unsafe_bins profile_bins unsafe_jbins profile_jbins checkconf


# Rules for building the safe, unsafe, and profile versions of objects and classes

define c_pattern_rule
.Olib_$1/%.o: $(SRC_DIR)/%.c
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$$(CC) -c $$(CFLAGS) $$< -o $$@ $$(LFLAGS) $$(LDFLAGS)                
endef

$(foreach V, $(VARIANT_SUFFIXES), $(eval $(call c_pattern_rule,$V)))


# The following rules for libs, bins, and test objects and classes are all very similar
# and I would prefer to have 1 set of rules for everything but have not found a way to
# do so and still support target specific arguments. TODO: Look into improving this.
define scm_lib_pattern_rules

LIB_$2_BFLAGS_$1 += $$(BFLAGS_$1)
LIB_$2_BCFLAGS_$1 += $$(BCFLAGS_$1)
LIB_$2_BLFLAGS_$1 += $$(BLFLAGS_$1)
LIB_$2_BLDFLAGS_$1 += $$(BLDFLAGS_$1)

.Olib_$1/lib/$2/%.o: $(SRC_DIR)/lib/$2/%.scm
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	$$(BIGLOO) $$(LIB_$2_BFLAGS_$1) $$(LIB_$2_BCFLAGS_$1:%=-copt %) $$(LIB_$2_BLFLAGS_$1) $$(LIB_$2_INCLUDE_FLAGS) $$(LIB_$2_C_INCLUDE_DIRS:%=-copt " -I %") $$< -o $$@ -c

.class_$1/bigloo/lib/$2/%.class: $(SRC_DIR)/lib/$2/%.scm
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	$$(BIGLOO) -jvm -jfile $(JFILE) $$(LIB_$2_BFLAGS_$1) $$(LIB_$2_BLFLAGS_$1) $$(LIB_$2_INCLUDE_FLAGS) $$<  -o $$@ -c
endef

define scm_bin_pattern_rules

BIN_$2_BFLAGS_$1 += $$(BFLAGS_$1)
BIN_$2_BCFLAGS_$1 += $$(BCFLAGS_$1)
BIN_$2_BLFLAGS_$1 += $$(BLFLAGS_$1)

.Olib_$1/bin/$2/%.o: $(SRC_DIR)/bin/$2/%.scm
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$$(BIGLOO) $$(BIN_$2_BFLAGS_$1) $$(BIN_$2_BCFLAGS_$1:%=-copt %) $$(BIN_$2_BLFLAGS_$1) $$($2_INCLUDE_FLAGS) $$($2_C_INCLUDE_DIRS:%=-copt " -I %") $$< -o $$@ -c

.class_$1/bigloo/bin/$2/%.class: $(SRC_DIR)/bin/$2/%.scm
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	$$(BIGLOO)  -jvm -jfile $(JFILE) $$(BIN_$2_BFLAGS_$1) $$(BIN_$2_BLFLAGS_$1) $$($2_INCLUDE_FLAGS) $$<  -o $$@ -c
endef

define scm_test_pattern_rules

TEST_$2_BFLAGS_$1 += $$(BFLAGS_$1)
TEST_$2_BCFLAGS_$1 += $$(BCFLAGS_$1)
TEST_$2_BLFLAGS_$1 += $$(BLFLAGS_$1)

.Olib_$1/$2/%.o: $(TEST_DIR)/$2/%.scm | release_libs
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$$(BIGLOO) $$(TEST_$2_BFLAGS_$1) $$(TEST_$2_BCFLAGS_$1:%=-copt %) $$(TEST_$2_BLFLAGS_$1) $$(TEST_$2_INCLUDE_FLAGS) $$< -o $$@ -c

.class_$1/bigloo/test/$2/%.class: $(TEST_DIR)/$2/%.scm | release_zips
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$$(BIGLOO) -jvm -jfile $(JFILE) $$(TEST_$2_BFLAGS_$1) $$(TEST_$2_BLFLAGS_$1) $$(TEST_$2_INCLUDE_FLAGS) $$< -o $$@ -c                   

endef

$(foreach V, $(VARIANT_SUFFIXES), $(foreach T, $(LIB_TARGETS), $(eval $(call scm_lib_pattern_rules,$V,$T))))

$(foreach V, $(VARIANT_SUFFIXES), $(foreach T, $(BIN_TARGETS), $(eval $(call scm_bin_pattern_rules,$V,$T))))

$(foreach V, $(VARIANT_SUFFIXES), $(foreach T, $(TEST_TARGETS), $(eval $(call scm_test_pattern_rules,$V,$T))))


RELEASE_BINS := $(foreach T, $(BIN_TARGETS), $(BUILD_BIN_DIR)/$T_s)

UNSAFE_BINS := $(foreach T, $(BIN_TARGETS), $(BUILD_BIN_DIR)/$T_u)

RELEASE_JBINS := $(foreach T, $(BIN_TARGETS), $(BUILD_BIN_DIR)/j$T_s)

UNSAFE_JBINS := $(foreach T, $(BIN_TARGETS), $(BUILD_BIN_DIR)/j$T_u)

PROFILE_BINS := $(foreach T, $(BIN_TARGETS), $(BUILD_BIN_DIR)/$T_p)

PROFILE_JBINS := $(foreach T, $(BIN_TARGETS), $(BUILD_BIN_DIR)/j$T_p)

TEST_BINS := $(foreach T, $(TEST_TARGETS), $(TEST_BUILD_DIR)/$T)
TEST_JBINS := $(foreach T, $(TEST_TARGETS), $(TEST_BUILD_DIR)/j$T)

define make_lib_targets
$(foreach T,$(LIB_TARGETS),$(BUILD_LIB_DIR)/lib$T_$1-$(VERSION).$(STATIC_LIBRARY_EXT) \
                           $(BUILD_LIB_DIR)/lib$T_$1-$(VERSION).$(SHARED_LIBRARY_EXT) \
                           $(BUILD_LIB_DIR)/lib$T_$1_mt-$(VERSION).$(SHARED_LIBRARY_EXT) \
                           $(BUILD_LIB_DIR)/lib$T_e$1-$(VERSION).$(SHARED_LIBRARY_EXT) \
                           $(BUILD_LIB_DIR)/lib$T_e$1_mt-$(VERSION).$(SHARED_LIBRARY_EXT))
endef

SAFE_LIBS := $(call make_lib_targets,s)

UNSAFE_LIBS := $(call make_lib_targets,u)

PROFILE_LIBS := $(call make_lib_targets,p)

RELEASE_LIBS := $(SAFE_LIBS) $(UNSAFE_LIBS) # $(PROFILE_LIBS)

# Make functions
define make_zip_targets
$(foreach T,$(LIB_TARGETS),$(BUILD_LIB_DIR)/$T_$1-$(VERSION).zip \
                           $(BUILD_LIB_DIR)/$T_e$1-$(VERSION).zip)
endef

SAFE_ZIPS := $(call make_zip_targets,s)

UNSAFE_ZIPS := $(call make_zip_targets,u)

PROFILE_ZIPS := $(call make_zip_targets,p)

RELEASE_ZIPS := $(SAFE_ZIPS) $(UNSAFE_ZIPS) #$(PROFILE_ZIPS)

all: checkconf release

checkconf:
	@ if ! [ -f "Makefile.config" ]; then \
	  echo "you must configure before building!"; \
	  exit 1; \
	fi

release: .etags .bee $(RELEASE_TARGETS)

release_bins: release_libs $(RELEASE_BINS)

release_jbins: release_zips $(RELEASE_JBINS) $(BUILD_LIB_DIR)/xz-1.9.jar $(BUILD_LIB_DIR)/zstd-jni-1.5.5-4.jar $(BUILD_LIB_DIR)/bzip2-0.9.1.jar

unsafe_jbins: $(BUILD_LIB_DIR)/zstd-jni-1.5.5-4.jar $(BUILD_LIB_DIR)/xz-1.9.jar $(UNSAFE_JBINS) $(BUILD_LIB_DIR)/bzip2-0.9.1.jar

unsafe_bins: $(UNSAFE_BINS)

profile_bins: $(PROFILE_BINS)

profile_jbins: $(PROFILE_JBINS)

release_libs: $(RELEASE_LIBS)

release_zips: $(BUILD_LIB_DIR)/zstd-jni-1.5.5-4.jar $(BUILD_LIB_DIR)/xz-1.9.jar $(BUILD_LIB_DIR)/bzip2-0.9.1.jar $(RELEASE_ZIPS) 

test_bins: $(TEST_BINS) 

test_jbins: $(BUILD_LIB_DIR)/zstd-jni-1.5.5-4.jar $(BUILD_LIB_DIR)/xz-1.9.jar $(BUILD_LIB_DIR)/bzip2-0.9.1.jar $(TEST_JBINS)  

define run_tests
	(TEST_FAILED=0;\
         for b in $1 ; do \
	   if [ -f "$$b" ]; then \
              echo "running $$b...";\
              LD_LIBRARY_PATH="$LD_LIBRARY_PATH:./$(BUILD_LIB_DIR)" $$b;\
              if [ "$$?" -ne "0" ]; then \
                 echo "$$b failed.";\
                 TEST_FAILED=-1; \
              fi; \
           fi; \
	done; \
        exit $$TEST_FAILED)
endef

test_native: $(TEST_BINS)
	@$(call run_tests,$(TEST_BINS))

test_jvm: $(TEST_JBINS) $(BUILD_LIB_DIR)/zstd-jni-1.5.5-4.jar $(BUILD_LIB_DIR)/xz-1.9.jar $(BUILD_LIB_DIR)/bzip2-0.9.1.jar
	@$(call run_tests,$(TEST_JBINS))

ACTIVE_TESTS=
ifeq ($(SUPPORT_NATIVE),true) 
	ACTIVE_TESTS += test_native
endif
ifeq ($(SUPPORT_JVM),true)
	ACTIVE_TESTS += test_jvm
endif

test: $(ACTIVE_TESTS)

$(BUILD_LIB_DIR)/zstd-jni-1.5.5-4.jar:
	@($(MAVEN) -q dependency:get -Ddest="$(@D)" -Dartifact=com.github.luben:zstd-jni:1.5.5-4) 

$(BUILD_LIB_DIR)/xz-1.9.jar:
	@($(MAVEN) -q dependency:get -Ddest="$(@D)" -Dartifact=org.tukaani:xz:1.9)

$(BUILD_LIB_DIR)/bzip2-0.9.1.jar:
	@($(MAVEN) -q dependency:get -Ddest="$(@D)" -Dartifact=org.itadaki:bzip2:0.9.1)

define java_bin_classes_rule
ifneq ($$($1_JAVA_CLASSES_$2),)
$$($1_JAVA_CLASSES_$2) &: $$($1_JAVA_SOURCES_$2)
	@(cd $(SRC_BIN_DIR)/$1/$(FFI_JAVA_SRC_DIR) && \
         $(JAVAC) -sourcepath "$(SRC_BIN_DIR)/$1/$(FFI_JAVA_SRC_DIR)" \
                  -classpath "$(CLASSPATH_$2):../../../../.class_$2" \
                  -d ../../../../.class_$2 $$(subst $(SRC_BIN_DIR)/$1/$(FFI_JAVA_SRC_DIR)/,,$$^))
endif
endef

$(foreach V, $(VARIANT_SUFFIXES), $(foreach T, $(BIN_TARGETS), $(eval $(call java_bin_classes_rule,$T,$V))))

define java_zip_classes_rule
ifneq ($$(ZIP_$1_JAVA_CLASSES_$2),)
$$(ZIP_$1_JAVA_CLASSES_$2) &: $$(ZIP_$1_JAVA_SOURCES_$2)
	(cd $(SRC_LIB_DIR)/$1/$(FFI_JAVA_SRC_DIR) && \
         $(JAVAC) -sourcepath "$(SRC_LIB_DIR)/$1/$(FFI_JAVA_SRC_DIR)" \
                  -classpath "$(CLASSPATH_$2):../../../../.class_$2" \
                  -d ../../../../.class_$2 $$(subst $(SRC_LIB_DIR)/$1/$(FFI_JAVA_SRC_DIR)/,,$$^))
endif
endef

$(foreach V, $(VARIANT_SUFFIXES), $(foreach T, $(LIB_TARGETS), $(eval $(call java_zip_classes_rule,$T,$V))))

define bin_TARGET_RULE
$(BUILD_BIN_DIR)/$1_$2: $(AFILE) $$($1_OBJECTS_$2) | release_libs
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$$(BIGLOO) -o $$@  $$(BIN_$1_BFLAGS_$2) $$(BIN_$1_BLFLAGS_$2) $$($1_INCLUDE_FLAGS) $$($1_OBJECTS_$2)
endef

$(foreach V, $(VARIANT_SUFFIXES), $(foreach T, $(BIN_TARGETS), $(eval $(call bin_TARGET_RULE,$T,$V))))

define jbin_TARGET_RULE
$(BUILD_BIN_DIR)/j$1_$2:  $$($1_CLASSES_$2) | $(JFILE) release_zips
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$$(JAR) $(JARFLAGS) $$@.jar $$($1_CLASSES_$2:.class_$2/%=-C .class_$2/ .)
	@$$(BIGLOO) -jvm  -jvm-classpath $$($1_CLASSPATH_$2) -jvm-opt $(JVMOPTFLAGS) -o $$@ -jfile $(JFILE) $$(BIN_$1_BFLAGS_$2) \
                    $$(BIN_$1_BLFLAGS_$2) $$($1_INCLUDE_FLAGS)  $$($1_CLASSES_$2:.class_$2/%=%)
endef

$(foreach V, $(VARIANT_SUFFIXES), $(foreach T, $(BIN_TARGETS), $(eval $(call jbin_TARGET_RULE,$T,$V))))

define test_bin_TARGET_RULE
$(TEST_BUILD_DIR)/$1: $(AFILE) $$(TEST_$1_OBJECTS) | release_libs
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$$(BIGLOO) -o $$@  $$(TEST_$1_BFLAGS_s) $$(TEST_$1_BLFLAGS_s) $$(TEST_$1_INCLUDE_FLAGS) $$(TEST_$1_OBJECTS)
endef

$(foreach T, $(TEST_TARGETS), $(eval $(call test_bin_TARGET_RULE,$T)))

define test_jbin_TARGET_RULE
$(TEST_BUILD_DIR)/j$1: $(AFILE) $(JFILE) $$(TEST_$1_CLASSES) | release_zips
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$$(JAR) $(JARFLAGS) $$@.jar $$(TEST_$1_CLASSES:.class_s/%=-C .class_s/ .)
	@$$(BIGLOO) -jvm  -jvm-classpath $$(TEST_$1_CLASSPATH) -jvm-opt $(JVMOPTFLAGS) -o $$@ -jfile $(JFILE) $$(TEST_$1_BFLAGS_s) \
                    $$(TEST_$1_BLFLAGS_s) $$(TEST_$1_INCLUDE_FLAGS)  $$(TEST_$1_CLASSES:.class_s/%=%)
endef

$(foreach T, $(TEST_TARGETS), $(eval $(call test_jbin_TARGET_RULE,$T)))

define lib_TARGET_RULES
$(BUILD_LIB_DIR)/lib$1_$2-$(VERSION).$(SHARED_LIBRARY_EXT): $(AFILE) $(BUILD_LIB_DIR)/$1.init $(BUILD_LIB_DIR)/$1.heap $$(LIB_$1_OBJECTS_$2)
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$(BIGLOO) -y -o $$@  $$(LIB_$1_BLFLAGS_$2) $$(LIB_$1_INCLUDE_FLAGS) -ldopt $$(LIB_$1_BLDFLAGS_$2)  -ldpostopt -Wl,-soname=`basename $$@` $$(LIB_$1_OBJECTS_$2)

$(BUILD_LIB_DIR)/lib$1_$2-$(VERSION).$(STATIC_LIBRARY_EXT): $(AFILE) $(BUILD_LIB_DIR)/$1.init $(BUILD_LIB_DIR)/$1.heap $$(LIB_$1_OBJECTS_$2) 
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$(AR) $(ARFLAGS) $$@ $$(LIB_$1_OBJECTS_$2)
	@$(RANLIB) $$@

$(BUILD_LIB_DIR)/lib$1_e$2-$(VERSION).$(SHARED_LIBRARY_EXT): $(AFILE) $(BUILD_LIB_DIR)/$1.init $(BUILD_LIB_DIR)/$1.heap $$(LIB_$1_HEAP_OBJECTS_$2)  
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$(BIGLOO) -y -o $$@  $$(LIB_$1_BLFLAGS_$2) $$(LIB_$1_INCLUDE_FLAGS) -ldopt $$(LIB_$1_BLDFLAGS_$2) -ldpostopt -Wl,-soname=`basename $$@` $$(LIB_$1_HEAP_OBJECTS_$2)

$(BUILD_LIB_DIR)/lib$1_$2_mt-$(VERSION).$(SHARED_LIBRARY_EXT): $(AFILE) $(BUILD_LIB_DIR)/$1.init $(BUILD_LIB_DIR)/$1.heap $$(LIB_$1_OBJECTS_$2) 
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$(BIGLOO) -y -o $$@ -eval '(set! *multi-threaded-gc?* #t)' \
                   $$(LIB_$1_BLFLAGS_$2) $$(LIB_$1_INCLUDE_FLAGS) -ldopt $$(LIB_$1_BLDFLAGS_$2)  -ldpostopt -Wl,-soname=`basename $$@` $$(LIB_$1_OBJECTS_$2)

$(BUILD_LIB_DIR)/lib$1_e$2_mt-$(VERSION).$(SHARED_LIBRARY_EXT): $(AFILE) $(BUILD_LIB_DIR)/$1.init $(BUILD_LIB_DIR)/$1.heap $$(LIB_$1_HEAP_OBJECTS_$2)
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$(BIGLOO) -y -o $$@ -eval '(set! *multi-threaded-gc?* #t)' \
                   $$(LIB_$1_BLFLAGS_$2) $$(LIB_$1_INCLUDE_FLAGS)  -ldopt $$(LIB_$1_BLDFLAGS_$2) -ldpostopt -Wl,-soname=`basename $$@` $$(LIB_$1_HEAP_OBJECTS_$2)
endef

$(foreach T,$(LIB_TARGETS),$(foreach V,$(VARIANT_SUFFIXES),$(eval $(call lib_TARGET_RULES,$T,$V))))

define zip_TARGET_RULES
$(BUILD_LIB_DIR)/$1_$2-$(VERSION).zip: $(AFILE) $(JFILE) $(BUILD_LIB_DIR)/$1.init $(BUILD_LIB_DIR)/$1.jheap $$(ZIP_$1_CLASSES_$2) $$(ZIP_$1_JAVA_CLASSES_$2) 
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$(JAR) $(JARFLAGS) $$@  $$(ZIP_$1_CLASSES_$2:.class_$2/%=-C .class_$2/ .)

$(BUILD_LIB_DIR)/$1_e$2-$(VERSION).zip: $(AFILE) $(JFILE) $(BUILD_LIB_DIR)/$1.init $(BUILD_LIB_DIR)/$1.jheap $$(ZIP_$1_HEAP_CLASSES_$2) 
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$(JAR) $(JARFLAGS) $$@ -C .class_$2/ bigloo/lib/$1/$(HEAP_SRC_BASE).class
endef

$(foreach T,$(LIB_TARGETS),$(foreach V,$(VARIANT_SUFFIXES),$(eval $(call zip_TARGET_RULES,$T,$V))))


# I would prefer to have a single pattern rule but don't see a way to collect
# the macro files correctly; I cannot capture % patterns in the find command used
# to gather the library macros.
define init_TARGET_RULES
$(BUILD_LIB_DIR)/$1.init: $(SRC_LIB_DIR)/$1/$(LIB_INIT_TEMPLATE)  $$(call find_files,$(SRC_LIB_DIR)/$1,'*.sch')
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@cat $$^ | $$(SED) -e "s|@VERSION@|$$(VERSION)|" > $$@
endef

$(foreach T, $(LIB_TARGETS), $(eval $(call init_TARGET_RULES,$T)))


# Generate the heap and jheap rules for each library target
define heap_TARGET_RULES
$(BUILD_LIB_DIR)/$1.heap: $(SRC_LIB_DIR)/$1/$(HEAP_SRC_BASE).scm
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$$(BIGLOO)  $$(HEAPFLAGS) $1 $$^ -addheap $$@ $$(LIB_$1_INCLUDE_FLAGS)

$(BUILD_LIB_DIR)/$1.jheap: $(SRC_LIB_DIR)/$1/$(HEAP_SRC_BASE).scm
	@echo "building $$@..."
	@if [ ! -d "$$(@D)" ]; then  mkdir -p $$(@D); fi
	@$$(BIGLOO) -jvm $$(HEAPFLAGS) $1 $$^ -addheap $$@ $$(LIB_$1_INCLUDE_FLAGS)
endef

$(foreach T,$(LIB_TARGETS),$(eval $(call heap_TARGET_RULES,$T)))

# find all scm files in project
SCM_SRC := $(call find_files,$(SRC_DIR),'*.scm') $(call find_files,$(TEST_DIR),'*.scm')

$(AFILE): $(SCM_SRC)
	@echo "building $@..."
	@$(RM) $@;
	@$(BGLAFILE) -o $@ $^

$(JFILE): $(SCM_SRC)
	@echo "building $@.."
	@$(RM) $@;
	@$(BGLJFILE) -pbase bigloo -o $@.temp  $^
	@cat $@.temp | sed -e "s|$(SRC_DIR).||" > $@
	@rm $@.temp

.etags: $(call find_files,$(SRC_DIR),'*.scm') \
        $(call find_files,$(SRC_DIR),'*.sch') \
        $(call find_files,$(SRC_DIR),'*.c') \
        $(call find_files,$(SRC_DIR),'*.java') 
	$(BGLTAGS) -o $@ $^

.bee:
	touch $@

check-syntax: $(AFILE) release_libs 
	$(BIGLOO) -c $(BFLAGS_s) $(BLFLAGS_s) -syntax-check ${CHK_SOURCES}

install: all
	@echo "installing..."
	@if [ -d $(BUILD_BIN_DIR) ]; then \
	    for file in $(BUILD_BIN_DIR)/*; do \
	    	$(INSTALL) $$file $(INSTALL_BIN_DIR)/`basename $$file`; \
	    done; \
	fi;
	@if [ -d $(BUILD_LIB_DIR) ]; then \
	    for file in $(BUILD_LIB_DIR)/*; do \
	  	$(INSTALL) $$file $(INSTALL_LIB_DIR)/`basename $$file`; \
	    done; \
	fi;

uninstall:
	@echo "uninstalling..."
	@if [ -d $(BUILD_BIN_DIR) ]; then \
           for file in $(BUILD_BIN_DIR)/*; do \
	  	$(RM) $(INSTALL_BIN_DIR)/`basename $$file`; \
	   done; \
        fi;
	@if [ -d $(BUILD_LIB_DIR) ]; then \
           for file in $(BUILD_LIB_DIR)/*; do \
	  	$(RM) $(INSTALL_LIB_DIR)/`basename $$file`; \
	   done; \
        fi;

distclean: clean
	@$(RM) Makefile.config

clean:
	@echo "removing build artifacts..."
	@$(RM) $(JFILE)
	@$(RM) $(AFILE)
	@$(RM) .etags
	@$(RM) $(BUILD_DIR)
	@$(RM) $(TEST_BUILD_DIR)
	@$(RM) ./.Olib_s
	@$(RM) ./.Olib_u
	@$(RM) ./.Olib_p
	@$(RM) ./.class_s
	@$(RM) ./.class_u
	@$(RM) ./.class_p

