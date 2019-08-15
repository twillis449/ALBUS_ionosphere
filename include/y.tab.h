
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     T_VEX_REV = 258,
     T_REF = 259,
     T_DEF = 260,
     T_ENDDEF = 261,
     T_SCAN = 262,
     T_ENDSCAN = 263,
     T_CHAN_DEF = 264,
     T_SAMPLE_RATE = 265,
     T_BITS_PER_SAMPLE = 266,
     T_SWITCHING_CYCLE = 267,
     T_START = 268,
     T_SOURCE = 269,
     T_MODE = 270,
     T_STATION = 271,
     T_DATA_TRANSFER = 272,
     T_ANTENNA_DIAM = 273,
     T_AXIS_OFFSET = 274,
     T_ANTENNA_MOTION = 275,
     T_POINTING_SECTOR = 276,
     T_AXIS_TYPE = 277,
     T_BBC_ASSIGN = 278,
     T_CLOCK_EARLY = 279,
     T_RECORD_TRANSPORT_TYPE = 280,
     T_ELECTRONICS_RACK_TYPE = 281,
     T_NUMBER_DRIVES = 282,
     T_HEADSTACK = 283,
     T_RECORD_DENSITY = 284,
     T_TAPE_LENGTH = 285,
     T_RECORDING_SYSTEM_ID = 286,
     T_TAPE_MOTION = 287,
     T_TAPE_CONTROL = 288,
     T_TAI_UTC = 289,
     T_A1_TAI = 290,
     T_EOP_REF_EPOCH = 291,
     T_NUM_EOP_POINTS = 292,
     T_EOP_INTERVAL = 293,
     T_UT1_UTC = 294,
     T_X_WOBBLE = 295,
     T_Y_WOBBLE = 296,
     T_EXPER_NUM = 297,
     T_EXPER_NAME = 298,
     T_EXPER_NOMINAL_START = 299,
     T_EXPER_NOMINAL_STOP = 300,
     T_PI_NAME = 301,
     T_PI_EMAIL = 302,
     T_CONTACT_NAME = 303,
     T_CONTACT_EMAIL = 304,
     T_SCHEDULER_NAME = 305,
     T_SCHEDULER_EMAIL = 306,
     T_TARGET_CORRELATOR = 307,
     T_EXPER_DESCRIPTION = 308,
     T_HEADSTACK_POS = 309,
     T_IF_DEF = 310,
     T_PASS_ORDER = 311,
     T_S2_GROUP_ORDER = 312,
     T_PHASE_CAL_DETECT = 313,
     T_TAPE_CHANGE = 314,
     T_NEW_SOURCE_COMMAND = 315,
     T_NEW_TAPE_SETUP = 316,
     T_SETUP_ALWAYS = 317,
     T_PARITY_CHECK = 318,
     T_TAPE_PREPASS = 319,
     T_PREOB_CAL = 320,
     T_MIDOB_CAL = 321,
     T_POSTOB_CAL = 322,
     T_HEADSTACK_MOTION = 323,
     T_PROCEDURE_NAME_PREFIX = 324,
     T_ROLL_REINIT_PERIOD = 325,
     T_ROLL_INC_PERIOD = 326,
     T_ROLL = 327,
     T_ROLL_DEF = 328,
     T_SEFD_MODEL = 329,
     T_SEFD = 330,
     T_SITE_TYPE = 331,
     T_SITE_NAME = 332,
     T_SITE_ID = 333,
     T_SITE_POSITION = 334,
     T_SITE_POSITION_EPOCH = 335,
     T_SITE_POSITION_REF = 336,
     T_SITE_VELOCITY = 337,
     T_HORIZON_MAP_AZ = 338,
     T_HORIZON_MAP_EL = 339,
     T_ZEN_ATMOS = 340,
     T_OCEAN_LOAD_VERT = 341,
     T_OCEAN_LOAD_HORIZ = 342,
     T_OCCUPATION_CODE = 343,
     T_INCLINATION = 344,
     T_ECCENTRICITY = 345,
     T_ARG_PERIGEE = 346,
     T_ASCENDING_NODE = 347,
     T_MEAN_ANOMALY = 348,
     T_SEMI_MAJOR_AXIS = 349,
     T_MEAN_MOTION = 350,
     T_ORBIT_EPOCH = 351,
     T_SOURCE_TYPE = 352,
     T_SOURCE_NAME = 353,
     T_IAU_NAME = 354,
     T_RA = 355,
     T_DEC = 356,
     T_SOURCE_POSITION_REF = 357,
     T_RA_RATE = 358,
     T_DEC_RATE = 359,
     T_SOURCE_POSITION_EPOCH = 360,
     T_REF_COORD_FRAME = 361,
     T_VELOCITY_WRT_LSR = 362,
     T_SOURCE_MODEL = 363,
     T_VSN = 364,
     T_FANIN_DEF = 365,
     T_FANOUT_DEF = 366,
     T_TRACK_FRAME_FORMAT = 367,
     T_DATA_MODULATION = 368,
     T_VLBA_FRMTR_SYS_TRK = 369,
     T_VLBA_TRNSPRT_SYS_TRK = 370,
     T_S2_RECORDING_MODE = 371,
     T_S2_DATA_SOURCE = 372,
     B_GLOBAL = 373,
     B_STATION = 374,
     B_MODE = 375,
     B_SCHED = 376,
     B_EXPER = 377,
     B_SCHEDULING_PARAMS = 378,
     B_PROCEDURES = 379,
     B_EOP = 380,
     B_FREQ = 381,
     B_CLOCK = 382,
     B_ANTENNA = 383,
     B_BBC = 384,
     B_CORR = 385,
     B_DAS = 386,
     B_HEAD_POS = 387,
     B_PASS_ORDER = 388,
     B_PHASE_CAL_DETECT = 389,
     B_ROLL = 390,
     B_IF = 391,
     B_SEFD = 392,
     B_SITE = 393,
     B_SOURCE = 394,
     B_TRACKS = 395,
     B_TAPELOG_OBS = 396,
     T_LITERAL = 397,
     T_NAME = 398,
     T_LINK = 399,
     T_ANGLE = 400,
     T_COMMENT = 401,
     T_COMMENT_TRAILING = 402
   };
#endif
/* Tokens.  */
#define T_VEX_REV 258
#define T_REF 259
#define T_DEF 260
#define T_ENDDEF 261
#define T_SCAN 262
#define T_ENDSCAN 263
#define T_CHAN_DEF 264
#define T_SAMPLE_RATE 265
#define T_BITS_PER_SAMPLE 266
#define T_SWITCHING_CYCLE 267
#define T_START 268
#define T_SOURCE 269
#define T_MODE 270
#define T_STATION 271
#define T_DATA_TRANSFER 272
#define T_ANTENNA_DIAM 273
#define T_AXIS_OFFSET 274
#define T_ANTENNA_MOTION 275
#define T_POINTING_SECTOR 276
#define T_AXIS_TYPE 277
#define T_BBC_ASSIGN 278
#define T_CLOCK_EARLY 279
#define T_RECORD_TRANSPORT_TYPE 280
#define T_ELECTRONICS_RACK_TYPE 281
#define T_NUMBER_DRIVES 282
#define T_HEADSTACK 283
#define T_RECORD_DENSITY 284
#define T_TAPE_LENGTH 285
#define T_RECORDING_SYSTEM_ID 286
#define T_TAPE_MOTION 287
#define T_TAPE_CONTROL 288
#define T_TAI_UTC 289
#define T_A1_TAI 290
#define T_EOP_REF_EPOCH 291
#define T_NUM_EOP_POINTS 292
#define T_EOP_INTERVAL 293
#define T_UT1_UTC 294
#define T_X_WOBBLE 295
#define T_Y_WOBBLE 296
#define T_EXPER_NUM 297
#define T_EXPER_NAME 298
#define T_EXPER_NOMINAL_START 299
#define T_EXPER_NOMINAL_STOP 300
#define T_PI_NAME 301
#define T_PI_EMAIL 302
#define T_CONTACT_NAME 303
#define T_CONTACT_EMAIL 304
#define T_SCHEDULER_NAME 305
#define T_SCHEDULER_EMAIL 306
#define T_TARGET_CORRELATOR 307
#define T_EXPER_DESCRIPTION 308
#define T_HEADSTACK_POS 309
#define T_IF_DEF 310
#define T_PASS_ORDER 311
#define T_S2_GROUP_ORDER 312
#define T_PHASE_CAL_DETECT 313
#define T_TAPE_CHANGE 314
#define T_NEW_SOURCE_COMMAND 315
#define T_NEW_TAPE_SETUP 316
#define T_SETUP_ALWAYS 317
#define T_PARITY_CHECK 318
#define T_TAPE_PREPASS 319
#define T_PREOB_CAL 320
#define T_MIDOB_CAL 321
#define T_POSTOB_CAL 322
#define T_HEADSTACK_MOTION 323
#define T_PROCEDURE_NAME_PREFIX 324
#define T_ROLL_REINIT_PERIOD 325
#define T_ROLL_INC_PERIOD 326
#define T_ROLL 327
#define T_ROLL_DEF 328
#define T_SEFD_MODEL 329
#define T_SEFD 330
#define T_SITE_TYPE 331
#define T_SITE_NAME 332
#define T_SITE_ID 333
#define T_SITE_POSITION 334
#define T_SITE_POSITION_EPOCH 335
#define T_SITE_POSITION_REF 336
#define T_SITE_VELOCITY 337
#define T_HORIZON_MAP_AZ 338
#define T_HORIZON_MAP_EL 339
#define T_ZEN_ATMOS 340
#define T_OCEAN_LOAD_VERT 341
#define T_OCEAN_LOAD_HORIZ 342
#define T_OCCUPATION_CODE 343
#define T_INCLINATION 344
#define T_ECCENTRICITY 345
#define T_ARG_PERIGEE 346
#define T_ASCENDING_NODE 347
#define T_MEAN_ANOMALY 348
#define T_SEMI_MAJOR_AXIS 349
#define T_MEAN_MOTION 350
#define T_ORBIT_EPOCH 351
#define T_SOURCE_TYPE 352
#define T_SOURCE_NAME 353
#define T_IAU_NAME 354
#define T_RA 355
#define T_DEC 356
#define T_SOURCE_POSITION_REF 357
#define T_RA_RATE 358
#define T_DEC_RATE 359
#define T_SOURCE_POSITION_EPOCH 360
#define T_REF_COORD_FRAME 361
#define T_VELOCITY_WRT_LSR 362
#define T_SOURCE_MODEL 363
#define T_VSN 364
#define T_FANIN_DEF 365
#define T_FANOUT_DEF 366
#define T_TRACK_FRAME_FORMAT 367
#define T_DATA_MODULATION 368
#define T_VLBA_FRMTR_SYS_TRK 369
#define T_VLBA_TRNSPRT_SYS_TRK 370
#define T_S2_RECORDING_MODE 371
#define T_S2_DATA_SOURCE 372
#define B_GLOBAL 373
#define B_STATION 374
#define B_MODE 375
#define B_SCHED 376
#define B_EXPER 377
#define B_SCHEDULING_PARAMS 378
#define B_PROCEDURES 379
#define B_EOP 380
#define B_FREQ 381
#define B_CLOCK 382
#define B_ANTENNA 383
#define B_BBC 384
#define B_CORR 385
#define B_DAS 386
#define B_HEAD_POS 387
#define B_PASS_ORDER 388
#define B_PHASE_CAL_DETECT 389
#define B_ROLL 390
#define B_IF 391
#define B_SEFD 392
#define B_SITE 393
#define B_SOURCE 394
#define B_TRACKS 395
#define B_TAPELOG_OBS 396
#define T_LITERAL 397
#define T_NAME 398
#define T_LINK 399
#define T_ANGLE 400
#define T_COMMENT 401
#define T_COMMENT_TRAILING 402




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 17 "vex.y"

int                     ival;
char                   *sval;
struct llist           *llptr;
struct qref            *qrptr;
struct def             *dfptr;
struct vex_block       *blptr;
struct lowl            *lwptr;
struct dvalue          *dvptr;
struct external        *exptr;

struct chan_def        *cdptr;
struct switching_cycle *scptr;

struct station         *snptr;
struct data_transfer   *dtptr;

struct axis_type       *atptr;
struct antenna_motion  *amptr;
struct pointing_sector *psptr;

struct bbc_assign      *baptr;

struct headstack       *hsptr;

struct clock_early     *ceptr;

struct tape_length     *tlptr;
struct tape_motion     *tmptr;

struct headstack_pos   *hpptr;

struct if_def          *ifptr;

struct phase_cal_detect *pdptr;

struct setup_always    *saptr;
struct parity_check    *pcptr;
struct tape_prepass    *tpptr;
struct preob_cal       *prptr;
struct midob_cal       *miptr;
struct postob_cal      *poptr;

struct sefd            *septr;

struct site_position   *spptr;
struct site_velocity   *svptr;
struct ocean_load_vert *ovptr;
struct ocean_load_horiz *ohptr;

struct source_model    *smptr;

struct vsn             *vsptr;

struct fanin_def	*fiptr;
struct fanout_def	*foptr;
struct vlba_frmtr_sys_trk	*fsptr;
struct s2_data_source  *dsptr;




/* Line 1676 of yacc.c  */
#line 409 "y.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


