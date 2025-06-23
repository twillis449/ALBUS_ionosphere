/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vex.h"

#define YYDEBUG 1

/* globals */

struct vex *vex_ptr=NULL;
extern int lines;

#line 85 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_HOME_NTSIKELELO_PROJECTS_ALBUS_ALBUS_IONOSPHERE_C_VEX_TO_SKY2_VEX2005_Y_TAB_H_INCLUDED
# define YY_YY_HOME_NTSIKELELO_PROJECTS_ALBUS_ALBUS_IONOSPHERE_C_VEX_TO_SKY2_VEX2005_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
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

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 17 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"

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


#line 346 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_HOME_NTSIKELELO_PROJECTS_ALBUS_ALBUS_IONOSPHERE_C_VEX_TO_SKY2_VEX2005_Y_TAB_H_INCLUDED  */



#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  9
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1442

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  151
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  275
/* YYNRULES -- Number of rules.  */
#define YYNRULES  658
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1372

#define YYUNDEFTOK  2
#define YYMAXUTOK   402


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   150,   149,
       2,   148,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   336,   336,   337,   339,   340,   342,   343,   344,   348,
     353,   354,   356,   357,   358,   359,   360,   361,   362,   363,
     364,   365,   366,   367,   368,   369,   370,   371,   372,   373,
     375,   376,   377,   378,   379,   383,   384,   388,   389,   391,
     392,   394,   395,   396,   398,   399,   403,   404,   406,   407,
     409,   410,   411,   413,   414,   419,   420,   422,   423,   424,
     426,   428,   429,   430,   431,   432,   433,   434,   435,   436,
     437,   438,   439,   440,   441,   442,   443,   444,   445,   446,
     447,   449,   450,   452,   453,   454,   456,   457,   459,   460,
     464,   465,   467,   468,   470,   471,   472,   474,   476,   478,
     479,   481,   482,   483,   484,   485,   486,   487,   489,   491,
     493,   495,   504,   511,   518,   519,   521,   522,   524,   525,
     527,   528,   529,   531,   532,   534,   535,   537,   538,   540,
     541,   543,   544,   549,   550,   552,   553,   555,   556,   557,
     559,   561,   563,   564,   566,   567,   568,   569,   570,   571,
     572,   573,   575,   577,   580,   582,   587,   598,   599,   601,
     602,   604,   605,   606,   608,   609,   612,   613,   615,   616,
     617,   618,   620,   625,   626,   628,   629,   631,   632,   633,
     635,   637,   640,   641,   643,   644,   645,   646,   648,   650,
     652,   654,   659,   660,   662,   663,   665,   666,   667,   669,
     670,   673,   674,   676,   677,   678,   679,   680,   681,   682,
     684,   685,   686,   687,   688,   690,   692,   694,   696,   698,
     701,   704,   706,   709,   711,   713,   715,   719,   723,   724,
     726,   727,   729,   730,   731,   733,   734,   737,   738,   740,
     741,   742,   743,   744,   745,   746,   747,   748,   749,   750,
     752,   754,   756,   758,   760,   762,   763,   765,   766,   768,
     769,   773,   774,   776,   777,   779,   780,   781,   783,   785,
     787,   788,   790,   791,   792,   793,   795,   797,   798,   799,
     800,   801,   802,   803,   805,   806,   807,   809,   811,   813,
     815,   817,   819,   821,   823,   825,   827,   829,   831,   835,
     836,   838,   839,   841,   842,   843,   845,   846,   849,   850,
     852,   853,   854,   855,   856,   857,   858,   860,   868,   876,
     884,   893,   894,   896,   898,   900,   902,   907,   908,   910,
     911,   913,   914,   915,   917,   919,   922,   923,   925,   926,
     927,   928,   930,   935,   936,   938,   939,   941,   942,   943,
     945,   946,   949,   950,   952,   953,   954,   955,   957,   959,
     961,   963,   965,   967,   972,   973,   975,   976,   979,   980,
     981,   983,   985,   988,   990,   992,   993,   995,   996,   997,
     999,  1001,  1005,  1006,  1008,  1010,  1012,  1013,  1014,  1016,
    1018,  1020,  1022,  1024,  1025,  1026,  1027,  1029,  1031,  1036,
    1037,  1039,  1041,  1043,  1044,  1045,  1047,  1049,  1052,  1054,
    1056,  1058,  1060,  1062,  1064,  1066,  1068,  1070,  1072,  1074,
    1076,  1078,  1079,  1080,  1082,  1084,  1086,  1088,  1090,  1093,
    1096,  1099,  1102,  1105,  1108,  1112,  1113,  1115,  1116,  1118,
    1119,  1120,  1122,  1124,  1127,  1128,  1130,  1131,  1132,  1133,
    1134,  1135,  1136,  1138,  1140,  1142,  1144,  1148,  1150,  1152,
    1154,  1157,  1158,  1159,  1161,  1163,  1166,  1168,  1171,  1172,
    1173,  1174,  1178,  1179,  1181,  1182,  1184,  1185,  1186,  1188,
    1190,  1193,  1194,  1196,  1197,  1198,  1199,  1200,  1202,  1204,
    1209,  1210,  1212,  1213,  1215,  1216,  1217,  1219,  1221,  1223,
    1224,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,  1234,
    1235,  1236,  1237,  1238,  1239,  1240,  1241,  1242,  1243,  1244,
    1245,  1246,  1247,  1248,  1249,  1251,  1253,  1255,  1257,  1261,
    1263,  1265,  1269,  1271,  1273,  1275,  1279,  1283,  1285,  1287,
    1289,  1291,  1293,  1295,  1297,  1299,  1303,  1304,  1306,  1307,
    1309,  1310,  1311,  1313,  1315,  1318,  1319,  1321,  1322,  1323,
    1324,  1325,  1326,  1327,  1328,  1329,  1330,  1331,  1332,  1333,
    1334,  1335,  1336,  1337,  1338,  1339,  1340,  1341,  1342,  1343,
    1345,  1346,  1349,  1351,  1353,  1355,  1357,  1359,  1361,  1363,
    1365,  1367,  1370,  1382,  1383,  1385,  1387,  1389,  1390,  1391,
    1394,  1396,  1399,  1401,  1403,  1404,  1405,  1406,  1409,  1414,
    1415,  1417,  1418,  1420,  1421,  1422,  1424,  1426,  1429,  1430,
    1432,  1433,  1434,  1436,  1437,  1439,  1441,  1442,  1443,  1444,
    1445,  1447,  1450,  1453,  1457,  1459,  1461,  1464,  1468,  1471,
    1473,  1475,  1478,  1480,  1485,  1488,  1490,  1491,  1493,  1494,
    1496,  1497,  1499,  1501,  1502,  1504,  1506,  1507,  1509
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "T_VEX_REV", "T_REF", "T_DEF",
  "T_ENDDEF", "T_SCAN", "T_ENDSCAN", "T_CHAN_DEF", "T_SAMPLE_RATE",
  "T_BITS_PER_SAMPLE", "T_SWITCHING_CYCLE", "T_START", "T_SOURCE",
  "T_MODE", "T_STATION", "T_DATA_TRANSFER", "T_ANTENNA_DIAM",
  "T_AXIS_OFFSET", "T_ANTENNA_MOTION", "T_POINTING_SECTOR", "T_AXIS_TYPE",
  "T_BBC_ASSIGN", "T_CLOCK_EARLY", "T_RECORD_TRANSPORT_TYPE",
  "T_ELECTRONICS_RACK_TYPE", "T_NUMBER_DRIVES", "T_HEADSTACK",
  "T_RECORD_DENSITY", "T_TAPE_LENGTH", "T_RECORDING_SYSTEM_ID",
  "T_TAPE_MOTION", "T_TAPE_CONTROL", "T_TAI_UTC", "T_A1_TAI",
  "T_EOP_REF_EPOCH", "T_NUM_EOP_POINTS", "T_EOP_INTERVAL", "T_UT1_UTC",
  "T_X_WOBBLE", "T_Y_WOBBLE", "T_EXPER_NUM", "T_EXPER_NAME",
  "T_EXPER_NOMINAL_START", "T_EXPER_NOMINAL_STOP", "T_PI_NAME",
  "T_PI_EMAIL", "T_CONTACT_NAME", "T_CONTACT_EMAIL", "T_SCHEDULER_NAME",
  "T_SCHEDULER_EMAIL", "T_TARGET_CORRELATOR", "T_EXPER_DESCRIPTION",
  "T_HEADSTACK_POS", "T_IF_DEF", "T_PASS_ORDER", "T_S2_GROUP_ORDER",
  "T_PHASE_CAL_DETECT", "T_TAPE_CHANGE", "T_NEW_SOURCE_COMMAND",
  "T_NEW_TAPE_SETUP", "T_SETUP_ALWAYS", "T_PARITY_CHECK", "T_TAPE_PREPASS",
  "T_PREOB_CAL", "T_MIDOB_CAL", "T_POSTOB_CAL", "T_HEADSTACK_MOTION",
  "T_PROCEDURE_NAME_PREFIX", "T_ROLL_REINIT_PERIOD", "T_ROLL_INC_PERIOD",
  "T_ROLL", "T_ROLL_DEF", "T_SEFD_MODEL", "T_SEFD", "T_SITE_TYPE",
  "T_SITE_NAME", "T_SITE_ID", "T_SITE_POSITION", "T_SITE_POSITION_EPOCH",
  "T_SITE_POSITION_REF", "T_SITE_VELOCITY", "T_HORIZON_MAP_AZ",
  "T_HORIZON_MAP_EL", "T_ZEN_ATMOS", "T_OCEAN_LOAD_VERT",
  "T_OCEAN_LOAD_HORIZ", "T_OCCUPATION_CODE", "T_INCLINATION",
  "T_ECCENTRICITY", "T_ARG_PERIGEE", "T_ASCENDING_NODE", "T_MEAN_ANOMALY",
  "T_SEMI_MAJOR_AXIS", "T_MEAN_MOTION", "T_ORBIT_EPOCH", "T_SOURCE_TYPE",
  "T_SOURCE_NAME", "T_IAU_NAME", "T_RA", "T_DEC", "T_SOURCE_POSITION_REF",
  "T_RA_RATE", "T_DEC_RATE", "T_SOURCE_POSITION_EPOCH",
  "T_REF_COORD_FRAME", "T_VELOCITY_WRT_LSR", "T_SOURCE_MODEL", "T_VSN",
  "T_FANIN_DEF", "T_FANOUT_DEF", "T_TRACK_FRAME_FORMAT",
  "T_DATA_MODULATION", "T_VLBA_FRMTR_SYS_TRK", "T_VLBA_TRNSPRT_SYS_TRK",
  "T_S2_RECORDING_MODE", "T_S2_DATA_SOURCE", "B_GLOBAL", "B_STATION",
  "B_MODE", "B_SCHED", "B_EXPER", "B_SCHEDULING_PARAMS", "B_PROCEDURES",
  "B_EOP", "B_FREQ", "B_CLOCK", "B_ANTENNA", "B_BBC", "B_CORR", "B_DAS",
  "B_HEAD_POS", "B_PASS_ORDER", "B_PHASE_CAL_DETECT", "B_ROLL", "B_IF",
  "B_SEFD", "B_SITE", "B_SOURCE", "B_TRACKS", "B_TAPELOG_OBS", "T_LITERAL",
  "T_NAME", "T_LINK", "T_ANGLE", "T_COMMENT", "T_COMMENT_TRAILING", "'='",
  "';'", "':'", "$accept", "vex", "version_lowls", "version_lowl",
  "version", "blocks", "block", "global_block", "station_block",
  "station_defs", "station_defx", "station_def", "mode_block", "mode_defs",
  "mode_defx", "mode_def", "refs", "refx", "ref", "primitive", "qrefs",
  "qrefx", "qref", "qualifiers", "sched_block", "sched_defs", "sched_defx",
  "sched_def", "sched_lowls", "sched_lowl", "start", "mode", "source",
  "station", "data_transfer", "start_position", "pass", "sector", "drives",
  "scan_id", "method", "destination", "unit_value2", "options",
  "antenna_block", "antenna_defs", "antenna_defx", "antenna_def",
  "antenna_lowls", "antenna_lowl", "antenna_diam", "axis_type",
  "axis_offset", "antenna_motion", "pointing_sector", "bbc_block",
  "bbc_defs", "bbc_defx", "bbc_def", "bbc_lowls", "bbc_lowl", "bbc_assign",
  "clock_block", "clock_defs", "clock_defx", "clock_def", "clock_lowls",
  "clock_lowl", "clock_early", "das_block", "das_defs", "das_defx",
  "das_def", "das_lowls", "das_lowl", "record_transport_type",
  "electronics_rack_type", "number_drives", "headstack", "record_density",
  "tape_length", "recording_system_id", "tape_motion", "tape_control",
  "eop_block", "eop_defs", "eop_defx", "eop_def", "eop_lowls", "eop_lowl",
  "tai_utc", "a1_tai", "eop_ref_epoch", "num_eop_points", "eop_interval",
  "ut1_utc", "x_wobble", "y_wobble", "exper_block", "exper_defs",
  "exper_defx", "exper_def", "exper_lowls", "exper_lowl", "exper_num",
  "exper_name", "exper_description", "exper_nominal_start",
  "exper_nominal_stop", "pi_name", "pi_email", "contact_name",
  "contact_email", "scheduler_name", "scheduler_email",
  "target_correlator", "freq_block", "freq_defs", "freq_defx", "freq_def",
  "freq_lowls", "freq_lowl", "chan_def", "switch_states", "switch_state",
  "sample_rate", "bits_per_sample", "switching_cycle", "head_pos_block",
  "head_pos_defs", "head_pos_defx", "head_pos_def", "head_pos_lowls",
  "head_pos_lowl", "headstack_pos", "if_block", "if_defs", "if_defx",
  "if_def", "if_lowls", "if_lowl", "if_def_st", "pass_order_block",
  "pass_order_defs", "pass_order_defx", "pass_order_def",
  "pass_order_lowls", "pass_order_lowl", "pass_order", "s2_group_order",
  "phase_cal_detect_block", "phase_cal_detect_defs",
  "phase_cal_detect_defx", "phase_cal_detect_def",
  "phase_cal_detect_lowls", "phase_cal_detect_lowl", "phase_cal_detect",
  "procedures_block", "procedures_defs", "procedures_defx",
  "procedures_def", "procedures_lowls", "procedures_lowl", "tape_change",
  "headstack_motion", "new_source_command", "new_tape_setup",
  "setup_always", "parity_check", "tape_prepass", "preob_cal", "midob_cal",
  "postob_cal", "procedure_name_prefix", "roll_block", "roll_defs",
  "roll_defx", "roll_def", "roll_lowls", "roll_lowl", "roll_reinit_period",
  "roll_inc_period", "roll", "roll_def_st", "scheduling_params_block",
  "scheduling_params_defs", "scheduling_params_defx",
  "scheduling_params_def", "scheduling_params_lowls",
  "scheduling_params_lowl", "sefd_block", "sefd_defs", "sefd_defx",
  "sefd_def", "sefd_lowls", "sefd_lowl", "sefd_model", "sefd",
  "site_block", "site_defs", "site_defx", "site_def", "site_lowls",
  "site_lowl", "site_type", "site_name", "site_id", "site_position",
  "site_position_epoch", "site_position_ref", "site_velocity",
  "horizon_map_az", "horizon_map_el", "zen_atmos", "ocean_load_vert",
  "ocean_load_horiz", "occupation_code", "inclination", "eccentricity",
  "arg_perigee", "ascending_node", "mean_anomaly", "semi_major_axis",
  "mean_motion", "orbit_epoch", "source_block", "source_defs",
  "source_defx", "source_def", "source_lowls", "source_lowl",
  "source_type", "source_name", "iau_name", "ra", "dec", "ref_coord_frame",
  "source_position_ref", "source_position_epoch", "ra_rate", "dec_rate",
  "velocity_wrt_lsr", "source_model", "tapelog_obs_block",
  "tapelog_obs_defs", "tapelog_obs_defx", "tapelog_obs_def",
  "tapelog_obs_lowls", "tapelog_obs_lowl", "vsn", "tracks_block",
  "tracks_defs", "tracks_defx", "tracks_def", "tracks_lowls",
  "tracks_lowl", "fanin_def", "fanout_def", "track_frame_format",
  "data_modulation", "vlba_frmtr_sys_trk", "vlba_trnsprt_sys_trk",
  "s2_recording_mode", "s2_data_source", "bit_stream_list", "external_ref",
  "literal", "unit_list", "unit_more", "unit_option", "unit_value",
  "name_list", "name_value", "value_list", "value", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,    61,    59,
      58
};
# endif

#define YYPACT_NINF (-1317)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      14,  -125, -1317, -1317,    35,   269, -1317, -1317,   -92, -1317,
     -72,   -62,    -9,    -6,    -4,     5,     8,    10,    15,    18,
      21,    41,    52,    61,    65,    81,    95,    97,   134,   137,
     148,   151,   153, -1317,   746, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
     160,    25,    87,   147,   200,   157,   161,   275,   406,   485,
     566,   571,   616,   633,   636,   641,   653,   657,   659,   666,
     668,   671,   673,   714, -1317, -1317,   776, -1317, -1317,    25,
   -1317, -1317,   -13, -1317, -1317,    87, -1317, -1317,   174, -1317,
   -1317,   147, -1317, -1317,   176, -1317, -1317,   200, -1317, -1317,
     183, -1317, -1317,   157, -1317, -1317,   192, -1317, -1317,   161,
   -1317, -1317,   211, -1317, -1317,   275, -1317, -1317,   219, -1317,
   -1317,   406, -1317, -1317,   221, -1317, -1317,   485, -1317, -1317,
     225, -1317, -1317,   566, -1317, -1317,   228, -1317, -1317,   571,
   -1317, -1317,   239, -1317, -1317,   616, -1317, -1317,   256, -1317,
   -1317,   633, -1317, -1317,   274, -1317, -1317,   636, -1317, -1317,
     277, -1317, -1317,   641, -1317, -1317,   280, -1317, -1317,   653,
   -1317, -1317,   285, -1317, -1317,   657, -1317, -1317,   288, -1317,
   -1317,   659, -1317, -1317,   321, -1317, -1317,   666, -1317, -1317,
     323, -1317, -1317,   668, -1317, -1317,   328, -1317, -1317,   671,
   -1317, -1317,   331, -1317, -1317,   673, -1317, -1317,   345, -1317,
   -1317,   714, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317,    47, -1317,   327, -1317,   370, -1317,
     397, -1317,   399, -1317,   416, -1317,   450, -1317,   453, -1317,
     456, -1317,   465, -1317,   477, -1317,   495, -1317,   504, -1317,
     507, -1317,   514, -1317,   523, -1317,   525, -1317,   528, -1317,
     530, -1317,   574, -1317,   576, -1317,   578, -1317,   579, -1317,
     363,   558,   631,   716,   135,   149,   159,   202,    69,   349,
      22,   600,    85,   316,   267,    46,   272,   374,   138,   357,
     594,   152,   377,   585,   586,   645,   776,   587, -1317, -1317,
     651, -1317, -1317,   588,   129,   212,   421,   462,   470, -1317,
   -1317,   813, -1317, -1317, -1317, -1317, -1317, -1317,   412,   589,
     502,   591,   607,   609,   610,   612,   613,   622,   623,   624,
     626,   627, -1317, -1317,   205, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,   593,
     596, -1317, -1317,   564, -1317, -1317, -1317,   617,   628,   642,
     660,   661,   662,   663,   674,   676,   677,   685,   686, -1317,
   -1317,   466, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317,   620,   687,   688,   689,   711,
     728,   740,   741,   742, -1317, -1317,   420, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317,   632,   743,   744,
     745,   747, -1317, -1317,   469, -1317, -1317, -1317, -1317, -1317,
   -1317,   635,   770, -1317, -1317,   618, -1317, -1317, -1317,   640,
     771,   772,   773,   774,   775, -1317, -1317,   128, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317,   644,   777, -1317, -1317,   605,
   -1317, -1317, -1317,   647,   778,   779,   780,   781,   782,   783,
     784,   785,   786, -1317, -1317,   483, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317,   652,   787, -1317,
   -1317,   621, -1317, -1317, -1317,   667,   788,   789, -1317, -1317,
     461, -1317, -1317, -1317, -1317,   790,   792, -1317, -1317,   487,
   -1317, -1317, -1317,   793,   795,   796,   797,   798, -1317, -1317,
     597, -1317, -1317, -1317, -1317, -1317, -1317,   799,   801, -1317,
   -1317,   648, -1317, -1317, -1317,   802,   804,   805, -1317, -1317,
     185, -1317, -1317, -1317, -1317,   806,   808,   809,   810,   814,
     815,   816,   817,   818,   819,   820,   821,   822,   823,   824,
     825,   826,   827,   828,   829,   830,   831, -1317, -1317,   501,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317,   812,   832,   833,   834,   835,   836,   837,
     838,   839,   840,   841,   842,   843, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317,   750, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317,   844,   846,   847,   848,   849,   850,   851,   852,   853,
   -1317, -1317,   426, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317,   854,   856, -1317, -1317,   639, -1317, -1317,
   -1317, -1317, -1317,   857,   859, -1317,   860, -1317, -1317,   577,
     751,   807,   811,   862,   861, -1317,   557, -1317,   -92,   865,
     868,   869,   870,   871,   872,   873,   874,   875,   876,   877,
     878, -1317, -1317, -1317,   879, -1317, -1317,   881,   881,   881,
     882,   882,   882,   882,   882,   882,   881,   883,   880, -1317,
   -1317,   881,   881,   887,   -92,   881,   -96,   -67,   -21,   884,
   -1317, -1317,   -19,   881,   -92,   888,   885, -1317, -1317,  -104,
     886, -1317, -1317,   881,   881,   889,   794,   893,   890, -1317,
   -1317,   803,   891, -1317, -1317,   894,   898,   -92,   -92,   899,
     881,   -92,   900,   901,   896, -1317, -1317,   -92,   897, -1317,
   -1317,   882,   -92,   902, -1317, -1317,   858,   903, -1317, -1317,
     881,   -92,   904,   -92,   906, -1317, -1317,   905,   907, -1317,
   -1317,   910,   913,   909, -1317, -1317,   916,   917,   918,   881,
     919,   920,   881,   881,   881,   881,   881,   881,   882,   881,
     -92,   881,   881,   881,   881,   -92,   921,   922, -1317, -1317,
     923,   924,   925,   926,   927,   930,   881,   881,   931,   932,
     881,   -92,   928, -1317, -1317,   933,   -41,   935,   936,   -92,
     -92,   937,   938,   934, -1317, -1317,   -92,   939, -1317, -1317,
     941, -1317,   940,   942,   943,   791, -1317,   915, -1317,   776,
     944,   945,   946,   947,   948,   950,   951,   952,   953,   954,
     955,   957, -1317, -1317,   964,   959,   960,   961, -1317,   962,
     963,   965,   966,   967,   968,   970,   971, -1317,   972,   973,
     974,   975,   976, -1317,   977,   978, -1317,   980, -1317,   981,
   -1317,   982,   881,   984,   985,   986, -1317,   987,   881, -1317,
     989,   991,   992,   993,   994, -1317,   995, -1317,   997,   998,
     999,  1000,   988,  -128,  1002,  -101,  1003, -1317,  1004, -1317,
     -64, -1317,   -52, -1317, -1317,   -14, -1317,  1006,  1007,  1008,
     -12, -1317,  1009, -1317,  1011,  1012, -1317,  1014,  1015,  1016,
    1017,  1019,  1020,  1021,  1023,  1024,  1025,  1026,  1027,  1029,
    1030,  1031,  1032,  1033,  1034,  1035,  1036,  1038, -1317,    24,
    1040,  1041,  1042,  1043,  1044,  1045,  1046,  1047,  1048,  1049,
    1050, -1317,  1053,  1054,   983,  1056,  1058,  1059,  1060,  1062,
      48, -1317,  1063, -1317,    50, -1317, -1317, -1317,   881,  1010,
    1001, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317,   881,   881,   881,
     881,   881,   881, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317,  1018, -1317, -1317,   881,  1064, -1317, -1317,   881,   881,
      54, -1317, -1317,   881,  1065,  1069,   -92, -1317, -1317, -1317,
     -24,  1066, -1317,  1073, -1317, -1317,   881, -1317,   881, -1317,
     882, -1317,   -92, -1317,   -92, -1317, -1317, -1317, -1317,  1074,
   -1317,   881, -1317, -1317, -1317,   881, -1317, -1317,   881, -1317,
   -1317, -1317,   881,   881, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317,  1075, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317,  1022,   -92,   983,  1070,  1071,
   -1317, -1317,  1076,   -92, -1317, -1317,  1055,  1080, -1317,  1081,
     139,  1077, -1317,  1078,  1082,  1083,  1084,  1085,  1079,  1086,
    1087,   964,  1088, -1317, -1317, -1317,  1089,  1092,  1091,   207,
   -1317,  1098,  1093,  1094,  1096,  1097,  1100,   -92, -1317,  1101,
     209,  1099, -1317, -1317,   217,  1102,  1103,  1104,  1105,  1107,
    1108,  1109,  1110,  1111,  1112,  1116,   231,  1113,  1115,  1118,
    1119, -1317, -1317,  1122,   881,  1123,  1121, -1317, -1317, -1317,
     882,   882,   882,  1018,  1128,  1124, -1317, -1317,  1129,  1125,
     881,   881, -1317,  1132,   -92,  1130,   -92, -1317,   881, -1317,
   -1317,  1134,   -92,   881,   881, -1317, -1317, -1317,   881,   -92,
     231, -1317,  1131,  1133,   -92, -1317,  1136,  1135, -1317,  1137,
   -1317,  1138, -1317,  1140,  1141,  1142, -1317,  1143,   881,  1144,
     -92,  1146,  1147,  1149,  1150, -1317,  1151,  1152,  1153,   235,
    1155,  1156,  1157,  1158,  1159,  1139,   -92,   333,  1161,  1162,
     881,  1163, -1317, -1317, -1317,   881,  1164,   -92,  1166, -1317,
     881, -1317, -1317, -1317,   881,   881, -1317, -1317, -1317,   881,
     983,   -92, -1317,   401, -1317,   -92, -1317,  1168,  1167, -1317,
    1170,  1169,  1171,  1148,  1173, -1317,  1174,  1176,  1177,  1178,
     407,   411, -1317,  1180,  1182,  1175, -1317,  1163,  1172,  1183,
   -1317,  1189, -1317,  1191,   -92, -1317,  1179, -1317, -1317, -1317,
   -1317,  1185,   425,  1186,  1193,  1188,   510,  1190,  1195, -1317,
    1198,  1200,  1192,   881, -1317,   -55,   881, -1317,  1196, -1317,
    1199,  1197,  1201,  1202, -1317,  1204,   516,  1205,   -92, -1317,
    1206,   532,   881, -1317, -1317,   -20,   881,  1207,  1208,   559,
   -1317,   -92,   565, -1317,  1210, -1317,  1211,  1212, -1317,   -92,
   -1317,   572, -1317, -1317, -1317, -1317, -1317,   881, -1317, -1317,
    1214, -1317
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     0,     7,     8,     0,     3,     5,     6,     0,     1,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     4,     2,    11,    12,    13,    14,    16,
      17,    18,    19,    20,    21,    22,    15,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,   658,
       0,    36,    38,    47,    91,   262,   458,   400,   229,   300,
     174,   134,   158,   193,   328,   365,   383,   436,   344,   473,
     491,   547,   610,   594,    10,     9,     0,    58,    59,    35,
      56,    57,     0,    42,    43,    37,    40,    41,     0,    51,
      52,    46,    49,    50,     0,    95,    96,    90,    93,    94,
       0,   266,   267,   261,   264,   265,     0,   462,   463,   457,
     460,   461,     0,   404,   405,   399,   402,   403,     0,   233,
     234,   228,   231,   232,     0,   304,   305,   299,   302,   303,
       0,   178,   179,   173,   176,   177,     0,   138,   139,   133,
     136,   137,     0,   162,   163,   157,   160,   161,     0,   197,
     198,   192,   195,   196,     0,   332,   333,   327,   330,   331,
       0,   369,   370,   364,   367,   368,     0,   387,   388,   382,
     385,   386,     0,   440,   441,   435,   438,   439,     0,   348,
     349,   343,   346,   347,     0,   477,   478,   472,   475,   476,
       0,   495,   496,   490,   493,   494,     0,   551,   552,   546,
     549,   550,     0,   614,   615,   609,   612,   613,     0,   598,
     599,   593,   596,   597,    61,    62,    63,    64,    65,    68,
      66,    67,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,     0,    55,     0,    39,     0,    48,
       0,    92,     0,   263,     0,   459,     0,   401,     0,   230,
       0,   301,     0,   175,     0,   135,     0,   159,     0,   194,
       0,   329,     0,   366,     0,   384,     0,   437,     0,   345,
       0,   474,     0,   492,     0,   548,     0,   611,     0,   595,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    84,    85,
       0,    82,    83,     0,     0,     0,     0,     0,     0,   106,
     107,     0,   100,   101,   102,   103,   104,   105,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   285,   286,     0,   271,   272,   273,   274,   275,
     276,   277,   278,   279,   280,   281,   282,   283,   284,     0,
       0,   470,   471,     0,   467,   468,   469,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   422,
     423,     0,   409,   410,   411,   412,   413,   414,   415,   416,
     417,   418,   419,   420,   421,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   248,   249,     0,   238,   239,   240,
     241,   242,   243,   244,   245,   246,   247,     0,     0,     0,
       0,     0,   315,   316,     0,   309,   310,   311,   312,   313,
     314,     0,     0,   186,   187,     0,   183,   184,   185,     0,
       0,     0,     0,     0,     0,   150,   151,     0,   143,   144,
     145,   146,   147,   148,   149,     0,     0,   170,   171,     0,
     167,   168,   169,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   213,   214,     0,   202,   203,   204,   205,
     206,   207,   208,   209,   210,   211,   212,     0,     0,   340,
     341,     0,   337,   338,   339,     0,     0,     0,   378,   379,
       0,   374,   375,   376,   377,     0,     0,   395,   396,     0,
     392,   393,   394,     0,     0,     0,     0,     0,   451,   452,
       0,   445,   446,   447,   448,   449,   450,     0,     0,   356,
     357,     0,   353,   354,   355,     0,     0,     0,   486,   487,
       0,   482,   483,   484,   485,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   523,   524,     0,
     500,   501,   502,   503,   504,   505,   506,   507,   508,   509,
     510,   511,   512,   513,   514,   515,   516,   517,   518,   519,
     520,   521,   522,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   578,   579,   569,   570,
     571,   572,   573,   574,   575,   576,     0,   556,   557,   558,
     559,   560,   561,   562,   563,   564,   565,   566,   567,   568,
     577,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     629,   630,     0,   619,   620,   621,   622,   623,   624,   625,
     626,   627,   628,     0,     0,   606,   607,     0,   603,   604,
     605,    60,    45,     0,     0,    54,     0,    81,    98,     0,
       0,     0,     0,   123,     0,    99,     0,   269,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   270,   465,   645,     0,   466,   407,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   408,
     236,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     237,   307,     0,     0,     0,     0,     0,   308,   181,     0,
       0,   182,   141,     0,     0,     0,     0,     0,     0,   142,
     165,     0,     0,   166,   200,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   201,   335,     0,     0,   336,
     372,     0,     0,     0,   373,   390,     0,     0,   391,   443,
       0,     0,     0,     0,     0,   444,   351,     0,     0,   352,
     480,     0,     0,     0,   481,   498,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   499,   554,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   555,   617,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   618,   601,     0,     0,   602,    44,
       0,    53,     0,     0,     0,     0,   124,     0,    97,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   268,   464,     0,     0,     0,     0,   655,     0,
       0,     0,     0,     0,     0,     0,     0,   406,     0,     0,
       0,     0,     0,   256,     0,   647,   258,     0,   260,     0,
     235,     0,     0,     0,     0,     0,   306,     0,     0,   180,
       0,     0,     0,     0,     0,   140,     0,   164,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   199,     0,   334,
       0,   654,     0,   657,   371,     0,   389,     0,     0,     0,
       0,   442,     0,   350,     0,     0,   479,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   497,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   553,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   616,     0,   600,     0,   108,   110,   109,     0,   125,
       0,   287,   288,   290,   291,   292,   293,   294,   295,   296,
     297,   298,   289,   652,   424,   426,   427,     0,     0,     0,
       0,     0,     0,   425,   434,   250,   251,   252,   253,   254,
     255,     0,   257,   259,     0,     0,   324,   325,     0,     0,
       0,   152,   154,     0,     0,     0,     0,   215,   216,   217,
       0,     0,   221,     0,   223,   224,     0,   227,     0,   380,
       0,   381,     0,   398,     0,   453,   454,   455,   456,     0,
     488,     0,   525,   526,   527,     0,   529,   530,     0,   532,
     533,   534,     0,     0,   537,   538,   539,   540,   541,   542,
     543,   544,   545,   580,     0,   582,   583,   584,   585,   587,
     589,   590,   588,   586,   591,     0,     0,     0,     0,     0,
     634,   635,     0,     0,   639,   641,     0,     0,    87,     0,
       0,     0,   126,     0,     0,     0,     0,     0,     0,     0,
       0,   658,   646,   649,   650,   651,     0,     0,     0,     0,
     188,     0,     0,     0,     0,     0,     0,     0,   220,     0,
       0,     0,   653,   656,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    89,    86,     0,     0,   127,     0,   428,   429,   430,
       0,     0,     0,     0,     0,     0,   326,   189,     0,     0,
       0,     0,   153,     0,     0,     0,     0,   225,     0,   342,
     397,     0,     0,     0,     0,   535,   536,   581,     0,     0,
       0,   643,     0,     0,     0,   638,     0,     0,    88,     0,
     128,     0,   644,     0,     0,     0,   648,     0,     0,     0,
       0,     0,     0,     0,     0,   219,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,   129,   431,   432,   433,     0,     0,     0,     0,   155,
       0,   172,   218,   222,     0,     0,   489,   528,   531,     0,
       0,     0,   642,     0,   637,     0,   640,     0,     0,   115,
       0,     0,     0,     0,     0,   191,     0,     0,     0,     0,
       0,     0,   633,     0,     0,   116,   130,   129,     0,     0,
     190,     0,   226,     0,     0,   631,     0,   632,   636,   608,
     117,     0,     0,     0,     0,     0,     0,     0,   118,   113,
     131,     0,     0,     0,   359,     0,     0,   119,     0,   132,
       0,     0,     0,     0,   361,     0,     0,     0,   120,   112,
       0,     0,     0,   360,   362,     0,     0,     0,   121,     0,
     319,     0,     0,   322,     0,   363,     0,     0,   111,     0,
     317,     0,   323,   320,   321,   156,   358,     0,   122,   318,
       0,   592
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1317, -1317, -1317,  1106, -1317, -1317,   958, -1317, -1317, -1317,
     990, -1317, -1317, -1317,   949, -1317,   730,   -80, -1317,  -312,
   -1317,   604, -1317, -1317, -1317, -1317,   979, -1317, -1317,   691,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317,  -249, -1317, -1317, -1317,  1215, -1317, -1317,   625,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317,  1165, -1317, -1317,
     601, -1317, -1317, -1317,  1218, -1317, -1317,   669, -1317, -1317,
   -1317,   929, -1317, -1317,   602, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317,  1039, -1317, -1317,   719,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
    1028, -1317, -1317,   892, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,  1217, -1317,
   -1317,   867, -1317,  -123, -1316, -1317, -1317, -1317, -1317, -1317,
    1117, -1317, -1317,   729, -1317, -1317, -1317,  1051, -1317, -1317,
     690, -1317, -1317, -1317,  1184, -1317, -1317,   763, -1317, -1317,
   -1317, -1317,  1187, -1317, -1317,   766, -1317, -1317, -1317,  1224,
   -1317, -1317,   895, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317,  1194, -1317, -1317,   845,
   -1317, -1317, -1317, -1317, -1317, -1317,  1246, -1317, -1317,   996,
   -1317, -1317,  1181, -1317, -1317,   855, -1317, -1317, -1317, -1317,
    1203, -1317, -1317,   863, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317, -1317, -1317,  -305,  -299,  -297,
    -296,  -292,  -290,  -286,  -285, -1317, -1317,  1209, -1317, -1317,
     700, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1317,  1160, -1317, -1317,   701, -1317,
   -1317, -1317,  1213, -1317, -1317,   715, -1317, -1317, -1317, -1317,
   -1317, -1317, -1317, -1317, -1095,  -240, -1317,  -720, -1317,   187,
    -706, -1317,  -681,  -777,    -8
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,     5,     6,     7,    34,    35,    36,    37,    95,
      96,    97,    38,   101,   102,   103,    89,    90,    91,   244,
     320,   321,   322,  1120,    39,   107,   108,   109,   331,   332,
     333,   334,   335,   336,   337,  1278,  1311,  1328,  1347,   857,
    1123,  1221,  1281,  1330,    40,   149,   150,   151,   457,   458,
     459,   460,   461,   462,   463,    41,   155,   156,   157,   469,
     470,   471,    42,   143,   144,   145,   445,   446,   447,    43,
     161,   162,   163,   485,   486,   487,   488,   489,   490,   491,
     492,   493,   494,   495,    44,   131,   132,   133,   416,   417,
     418,   419,   420,   421,   422,   423,   424,   425,    45,   113,
     114,   115,   354,   355,   356,   357,   358,   359,   360,   361,
     362,   363,   364,   365,   366,   367,    46,   137,   138,   139,
     434,   435,   436,  1352,  1353,   437,   438,   439,    47,   167,
     168,   169,   501,   502,   503,    48,   191,   192,   193,   541,
     542,   543,    49,   173,   174,   175,   510,   511,   512,   513,
      50,   179,   180,   181,   519,   520,   521,    51,   125,   126,
     127,   391,   392,   393,   394,   395,   396,   397,   398,   399,
     400,   401,   402,   403,    52,   185,   186,   187,   530,   531,
     532,   533,   534,   535,    53,   119,   120,   121,   373,   374,
      54,   197,   198,   199,   550,   551,   552,   553,    55,   203,
     204,   205,   579,   580,   581,   582,   583,   584,   585,   586,
     587,   588,   589,   590,   591,   592,   593,   594,   595,   596,
     597,   598,   599,   600,   601,    56,   209,   210,   211,   626,
     627,   628,   629,   630,   631,   632,   633,   634,   635,   636,
     637,   638,   639,    57,   221,   222,   223,   667,   668,   669,
      58,   215,   216,   217,   652,   653,   654,   655,   656,   657,
     658,   659,   660,   661,  1109,   368,   376,   894,  1132,  1133,
     895,   930,   879,   932,   933
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      60,   875,   876,   877,   674,   618,   940,   897,   899,   245,
     885,   619,  1164,   620,   621,   888,   889,     1,   622,   892,
     623,  1052,  1053,     8,   624,   625,   338,   903,   449,    86,
     880,   881,   882,   883,   884,     9,  1364,   910,   911,   907,
     450,   451,   452,   453,   454,  1364,   908,   874,  1055,  1056,
     338,    59,   515,   893,   923,   375,   404,   426,   440,   448,
     464,   472,   496,   504,   514,   522,   536,   544,   554,   602,
     640,   662,   670,   338,   937,   427,   874,    61,   428,   429,
     430,   431,   896,   954,   955,  1059,  1060,    62,   874,   338,
     931,   473,    92,   950,  1334,  1335,   953,  1061,  1062,   956,
     957,   958,   983,   960,   516,   962,   963,   964,   965,   984,
     474,   475,   476,   477,   478,   479,   480,   481,   482,  1146,
     975,   976,   874,   874,   979,   901,  1147,   959,   898,  1355,
     246,   902,   338,   375,   748,  1063,  1064,  1068,  1062,   338,
      63,   339,   338,    64,   545,    65,   450,   451,   452,   453,
     454,   404,    98,   338,    66,   369,   338,    67,   641,    68,
       2,     3,   110,   338,    69,   377,   116,    70,   455,   456,
      71,    87,    88,  1093,  1094,  1290,   426,   340,   341,   342,
     343,   344,   345,   346,   347,   348,   349,   350,   351,   338,
      72,   793,   517,   518,   440,   290,  1035,  1115,  1116,  1118,
    1119,    73,  1040,  1140,  1141,   448,   338,   104,   405,   338,
      74,   700,   546,   547,    75,   432,   433,   464,   378,   379,
     380,   381,   382,   383,   384,   385,   386,   387,   388,   472,
      76,   483,   484,    93,    94,   245,   406,   407,   408,   409,
     410,   411,   412,   413,    77,   496,    78,   340,   341,   342,
     343,   344,   345,   346,   347,   348,   349,   350,   351,   546,
     547,   504,   642,   643,   644,   645,   646,   647,   648,   649,
     514,   338,     1,   505,   455,   456,   338,   679,   523,   522,
     122,   352,   353,    79,   548,   549,    80,  1154,  1172,  1173,
     536,   370,  1121,    99,   100,   371,   372,    81,   650,   651,
      82,   544,    83,   111,   112,   389,   390,   117,   118,    85,
     554,  1125,  1126,  1127,  1128,  1129,  1130,   248,  1138,   250,
     338,   618,   497,   506,   507,  1134,   252,   619,  1136,   620,
     621,   548,   549,  1139,   622,   254,   623,  1142,  1151,   602,
     624,   625,   524,   525,   526,   527,   105,   106,   414,   415,
    1150,   352,   353,   338,   256,   441,  1187,  1188,  1197,  1198,
     680,   338,   258,   555,   260,  1156,  1200,  1062,   262,  1157,
     498,   264,  1158,   442,    59,  1212,  1159,  1160,   338,  1152,
     537,   338,   266,   663,  1266,  1062,   640,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,   268,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,   128,   662,   508,   509,     2,     3,   270,   528,   529,
     272,   123,   124,   274,   338,  1239,   729,   670,   276,   538,
     338,   278,   843,   556,   557,   558,   559,   560,   561,   562,
     563,   564,   565,   566,   567,   568,   569,   570,   571,   572,
     573,   574,   575,   576,   406,   407,   408,   409,   410,   411,
     412,   413,   499,   500,   280,   338,   282,   773,  1219,  1273,
     338,   284,   718,   338,   286,   736,   291,  1134,   428,   429,
     430,   431,  1274,  1275,  1231,  1232,   664,   338,   288,   764,
     134,   338,  1237,   777,  1291,   443,   444,  1240,  1241,  1223,
    1224,  1225,  1242,   577,   578,   338,   313,   817,   474,   475,
     476,   477,   478,   479,   480,   481,   482,   506,   507,   292,
     539,   540,  1256,   665,   666,   378,   379,   380,   381,   382,
     383,   384,   385,   386,   387,   388,   642,   643,   644,   645,
     646,   647,   648,   649,  1279,   516,   293,  1000,   294,  1282,
    1292,  1062,   129,   130,  1286,   686,  1305,  1306,  1287,  1288,
    1307,  1062,    86,  1289,   314,   295,   414,   415,   338,   681,
     704,   140,   650,   651,  1319,  1320,   146,   556,   557,   558,
     559,   560,   561,   562,   563,   564,   565,   566,   567,   568,
     569,   570,   571,   572,   573,   574,   575,   576,   338,   296,
     603,   338,   297,   784,   338,   298,   465,   508,   509,   338,
     682,   752,   389,   390,   299,   432,   433,  1333,   683,  1336,
    1337,   152,   338,   466,   740,   338,   300,   768,   466,   483,
     484,   135,   136,   517,   518,   316,  1354,   317,   158,  1356,
    1357,   164,   442,   338,   301,   847,   170,   577,   578,    86,
     688,   673,   338,   302,   788,   316,   303,   676,   176,  1324,
    1325,  1370,   182,   304,   188,  1344,  1345,   524,   525,   526,
     527,   194,   305,   200,   306,   498,   206,   307,   212,   308,
     860,  1350,  1351,   569,   570,   571,   572,   573,   574,   575,
     576,   604,   605,   606,   607,   608,   609,   610,   611,   612,
     613,   614,   615,   538,    87,    88,   370,   859,  1360,  1351,
     371,   372,   141,   142,  1363,  1351,   891,   147,   148,   218,
     852,  1369,  1351,   309,   323,   310,   904,   311,   312,   324,
     325,   326,   327,   328,   671,   672,   675,   678,   687,   689,
     616,   617,   702,   528,   529,   703,   467,   468,   664,   920,
     921,   467,   468,   924,   338,   690,   832,   691,   692,   928,
     693,   694,   153,   154,   443,   444,   706,   499,   500,   720,
     695,   696,   697,   938,   698,   699,   707,   318,   319,   159,
     160,   731,   165,   166,   738,   665,   666,   171,   172,   742,
     708,    87,    88,   750,   539,   540,   754,   318,   319,   177,
     178,   766,   961,   183,   184,   189,   190,   966,   709,   710,
     711,   712,   195,   196,   201,   202,   770,   207,   208,   213,
     214,   684,   713,   980,   714,   715,   324,   325,   326,   327,
     328,   987,   988,   716,   717,   721,   722,   723,   992,   569,
     570,   571,   572,   573,   574,   575,   576,   604,   605,   606,
     607,   608,   609,   610,   611,   612,   613,   614,   615,   724,
     219,   220,   329,   330,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,   725,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,   726,   727,
     728,   732,   733,   734,   853,   735,   616,   617,   224,   225,
     226,   227,   228,   229,   230,   231,   232,   233,   234,   235,
     236,   237,   238,   239,   240,   241,   242,   243,   739,   743,
     744,   745,   746,   747,   677,   751,   755,   756,   757,   758,
     759,   760,   761,   762,   763,   767,   771,   772,   913,   775,
     776,   998,   779,   780,   781,   782,   783,   916,   786,   787,
     854,   790,   791,   792,   855,   795,   796,   797,   798,   329,
     330,   819,   799,   800,   801,   802,   803,   804,   805,   806,
     807,   808,   809,   810,   811,   812,   813,   814,   815,   816,
     820,   821,   822,   823,   824,   825,   826,   827,   828,   829,
     830,   831,    84,   834,   835,   836,   837,   838,   839,   840,
     841,   842,   935,   845,   846,   856,   849,   850,   861,   851,
     858,   862,   863,   864,   865,   866,   867,   868,   869,   870,
     871,   315,   685,  1135,   874,   878,   886,   872,   873,   887,
     890,   905,   912,   900,   906,   909,   914,   918,  1145,   915,
     917,   919,   922,   925,   926,   927,   929,   939,  1312,   942,
     249,   934,   936,   944,  1153,   941,   943,   945,   946,   947,
     948,   949,   951,   952,   967,   999,   969,   970,   971,   972,
     753,   968,   973,   974,   977,   978,   982,   981,   985,   986,
     989,   990,   749,   991,   994,   247,   251,   765,   993,   995,
     269,   996,   997,  1001,  1002,  1003,  1004,  1005,  1163,  1006,
    1007,  1008,  1009,  1010,  1011,  1168,  1012,  1013,  1014,  1015,
    1016,    33,  1017,  1018,   741,  1019,  1020,  1021,  1022,  1023,
    1024,  1025,  1026,  1027,  1028,  1029,  1030,  1108,  1031,  1032,
    1033,  1051,  1034,  1036,  1037,   730,  1038,  1039,  1041,  1195,
    1042,   253,  1043,  1044,  1045,  1046,  1047,  1048,  1049,  1124,
    1050,  1054,  1057,  1122,  1058,  1065,  1066,  1067,  1213,  1069,
    1070,  1131,  1071,  1072,  1073,  1074,  1162,  1075,  1076,  1077,
     259,  1078,  1079,  1080,  1081,  1135,  1082,  1083,  1084,  1085,
    1086,  1087,  1088,  1089,  1090,  1091,  1234,  1092,  1236,  1095,
    1096,  1097,  1098,  1099,  1100,  1101,  1102,  1103,  1104,  1169,
    1105,  1243,  1244,  1106,  1107,  1110,  1247,  1111,  1143,  1112,
    1113,  1114,  1144,  1117,  1137,  1148,  1149,  1155,  1161,  1167,
    1165,  1166,  1258,  1170,  1171,  1176,  1361,  1174,  1175,  1180,
     769,   789,  1177,  1178,  1179,  1185,  1181,  1182,  1183,  1184,
    1186,  1189,   279,  1190,  1191,  1192,   701,  1193,  1199,  1284,
    1194,  1196,  1201,  1202,  1203,  1204,  1205,  1206,  1207,  1211,
    1208,  1209,  1210,  1214,  1215,  1218,  1220,  1293,  1216,  1217,
    1222,  1227,  1229,   774,  1228,  1230,  1233,  1238,  1249,  1235,
    1248,  1245,  1272,  1246,   271,   778,   719,  1250,  1251,  1252,
    1253,  1254,  1299,  1255,  1257,  1259,  1317,  1260,  1261,  1262,
    1263,   737,  1264,  1265,  1267,  1268,  1280,  1269,  1270,  1271,
    1276,  1294,  1277,  1296,  1283,  1285,  1313,  1295,  1310,  1297,
     267,  1298,  1300,  1212,  1301,  1302,   833,  1303,  1304,  1308,
    1348,  1309,  1315,  1314,  1316,  1318,  1321,  1322,  1323,  1327,
    1326,  1329,  1332,  1362,  1331,  1341,  1338,  1340,  1339,   257,
    1349,  1368,  1342,  1343,   261,  1346,  1358,   273,  1359,  1365,
    1366,   263,  1367,  1371,   265,   255,   275,   844,   848,   705,
    1226,     0,     0,     0,     0,   785,     0,     0,   281,   277,
       0,   289,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   794,   283,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   285,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   287,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   818
};

static const yytype_int16 yycheck[] =
{
       8,   707,   708,   709,   316,   310,   783,   727,   728,    89,
     716,   310,  1107,   310,   310,   721,   722,     3,   310,   725,
     310,   149,   150,   148,   310,   310,     4,   733,     6,     4,
     711,   712,   713,   714,   715,     0,  1352,   743,   744,   143,
      18,    19,    20,    21,    22,  1361,   150,   143,   149,   150,
       4,   143,     6,   149,   760,   295,   296,   297,   298,   299,
     300,   301,   302,   303,   304,   305,   306,   307,   308,   309,
     310,   311,   312,     4,   780,     6,   143,   149,     9,    10,
      11,    12,   149,   803,   804,   149,   150,   149,   143,     4,
     771,     6,     5,   799,   149,   150,   802,   149,   150,   805,
     806,   807,   143,   809,    58,   811,   812,   813,   814,   150,
      25,    26,    27,    28,    29,    30,    31,    32,    33,   143,
     826,   827,   143,   143,   830,   144,   150,   808,   149,   149,
     143,   150,     4,   373,     6,   149,   150,   149,   150,     4,
     149,     6,     4,   149,     6,   149,    18,    19,    20,    21,
      22,   391,     5,     4,   149,     6,     4,   149,     6,   149,
     146,   147,     5,     4,   149,     6,     5,   149,   146,   147,
     149,   146,   147,   149,   150,  1270,   416,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,     4,
     149,     6,   146,   147,   434,   148,   902,   149,   150,   149,
     150,   149,   908,   149,   150,   445,     4,     7,     6,     4,
     149,     6,    74,    75,   149,   146,   147,   457,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,   469,
     149,   146,   147,   146,   147,   315,    34,    35,    36,    37,
      38,    39,    40,    41,   149,   485,   149,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    74,
      75,   501,   110,   111,   112,   113,   114,   115,   116,   117,
     510,     4,     3,     6,   146,   147,     4,   148,     6,   519,
       5,   146,   147,   149,   146,   147,   149,  1064,   149,   150,
     530,   142,   998,   146,   147,   146,   147,   149,   146,   147,
     149,   541,   149,   146,   147,   146,   147,   146,   147,   149,
     550,  1017,  1018,  1019,  1020,  1021,  1022,   143,  1038,   143,
       4,   626,     6,    56,    57,  1031,   143,   626,  1034,   626,
     626,   146,   147,  1039,   626,   143,   626,  1043,  1058,   579,
     626,   626,    70,    71,    72,    73,   146,   147,   146,   147,
    1056,   146,   147,     4,   143,     6,   149,   150,   149,   150,
     148,     4,   143,     6,   143,  1071,   149,   150,   143,  1075,
      54,   143,  1078,    24,   143,   144,  1082,  1083,     4,  1060,
       6,     4,   143,     6,   149,   150,   626,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   143,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,     5,   652,   146,   147,   146,   147,   143,   146,   147,
     143,   146,   147,   143,     4,  1202,     6,   667,   143,    55,
       4,   143,     6,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    34,    35,    36,    37,    38,    39,
      40,    41,   146,   147,   143,     4,   143,     6,  1174,  1246,
       4,   143,     6,     4,   143,     6,   149,  1183,     9,    10,
      11,    12,   149,   150,  1190,  1191,   109,     4,   143,     6,
       5,     4,  1198,     6,  1271,   146,   147,  1203,  1204,  1180,
    1181,  1182,  1208,   146,   147,     4,   143,     6,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    56,    57,   149,
     146,   147,  1228,   146,   147,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,   110,   111,   112,   113,
     114,   115,   116,   117,  1250,    58,   149,   859,   149,  1255,
     149,   150,   146,   147,  1260,   143,   149,   150,  1264,  1265,
     149,   150,     4,  1269,     6,   149,   146,   147,     4,   148,
       6,     5,   146,   147,   149,   150,     5,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,     4,   149,
       6,     4,   149,     6,     4,   149,     6,   146,   147,     4,
     148,     6,   146,   147,   149,   146,   147,  1323,   148,  1325,
    1326,     5,     4,    23,     6,     4,   149,     6,    23,   146,
     147,   146,   147,   146,   147,     4,  1342,     6,     5,  1345,
    1346,     5,    24,     4,   149,     6,     5,   146,   147,     4,
     148,     6,     4,   149,     6,     4,   149,     6,     5,   149,
     150,  1367,     5,   149,     5,   149,   150,    70,    71,    72,
      73,     5,   149,     5,   149,    54,     5,   149,     5,   149,
     688,   149,   150,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,    55,   146,   147,   142,   150,   149,   150,
     146,   147,   146,   147,   149,   150,   724,   146,   147,     5,
     143,   149,   150,   149,     8,   149,   734,   149,   149,    13,
      14,    15,    16,    17,   149,   149,   149,   149,   149,   148,
     146,   147,   149,   146,   147,   149,   146,   147,   109,   757,
     758,   146,   147,   761,     4,   148,     6,   148,   148,   767,
     148,   148,   146,   147,   146,   147,   149,   146,   147,   149,
     148,   148,   148,   781,   148,   148,   148,   146,   147,   146,
     147,   149,   146,   147,   149,   146,   147,   146,   147,   149,
     148,   146,   147,   149,   146,   147,   149,   146,   147,   146,
     147,   149,   810,   146,   147,   146,   147,   815,   148,   148,
     148,   148,   146,   147,   146,   147,   149,   146,   147,   146,
     147,     8,   148,   831,   148,   148,    13,    14,    15,    16,
      17,   839,   840,   148,   148,   148,   148,   148,   846,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   148,
     146,   147,   146,   147,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   148,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   148,   148,
     148,   148,   148,   148,   143,   148,   146,   147,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   148,   148,
     148,   148,   148,   148,   320,   148,   148,   148,   148,   148,
     148,   148,   148,   148,   148,   148,   148,   148,   144,   149,
     148,   150,   149,   148,   148,   148,   148,   144,   149,   148,
     143,   149,   148,   148,   143,   149,   148,   148,   148,   146,
     147,   149,   148,   148,   148,   148,   148,   148,   148,   148,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   148,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   148,
     148,   148,    34,   149,   148,   148,   148,   148,   148,   148,
     148,   148,   144,   149,   148,   143,   149,   148,   143,   149,
     149,   143,   143,   143,   143,   143,   143,   143,   143,   143,
     143,   291,   331,  1031,   143,   143,   143,   149,   149,   149,
     143,   143,   143,   149,   149,   149,   143,   143,  1046,   149,
     149,   143,   143,   143,   143,   149,   149,   143,  1297,   144,
     101,   149,   149,   143,  1062,   149,   149,   144,   149,   143,
     143,   143,   143,   143,   143,   150,   143,   143,   143,   143,
     469,   149,   145,   143,   143,   143,   143,   149,   143,   143,
     143,   143,   457,   149,   143,    95,   107,   485,   149,   149,
     161,   149,   149,   149,   149,   149,   149,   149,  1106,   149,
     149,   149,   149,   149,   149,  1113,   149,   143,   149,   149,
     149,     5,   150,   150,   445,   150,   150,   150,   150,   149,
     149,   149,   149,   149,   149,   149,   149,   144,   150,   149,
     149,   143,   150,   149,   149,   416,   150,   150,   149,  1147,
     149,   113,   150,   150,   150,   150,   149,   149,   149,   148,
     150,   149,   149,   143,   150,   149,   149,   149,  1166,   150,
     149,   143,   150,   149,   149,   149,   144,   150,   149,   149,
     131,   150,   149,   149,   149,  1183,   150,   150,   149,   149,
     149,   149,   149,   149,   149,   149,  1194,   149,  1196,   149,
     149,   149,   149,   149,   149,   149,   149,   149,   149,   144,
     150,  1209,  1210,   150,   150,   149,  1214,   149,   143,   150,
     150,   149,   143,   150,   150,   149,   143,   143,   143,   143,
     150,   150,  1230,   143,   143,   143,  1349,   150,   150,   150,
     501,   541,   149,   149,   149,   143,   150,   150,   150,   150,
     149,   143,   191,   150,   150,   149,   354,   150,   149,  1257,
     150,   150,   150,   150,   150,   150,   149,   149,   149,   143,
     150,   150,   150,   150,   149,   143,   143,  1275,   150,   150,
     149,   143,   143,   510,   150,   150,   144,   143,   143,   149,
     144,   150,   143,   150,   167,   519,   391,   150,   150,   149,
     149,   149,   144,   150,   150,   149,  1304,   150,   149,   149,
     149,   434,   150,   150,   149,   149,   143,   150,   150,   150,
     149,   143,   150,   143,   150,   149,   144,   150,   143,   150,
     155,   150,   149,   144,   150,   149,   626,   150,   150,   149,
    1338,   149,   143,   150,   143,   150,   150,   144,   150,   144,
     150,   143,   150,  1351,   144,   144,   150,   150,   149,   125,
     144,  1359,   150,   149,   137,   150,   149,   173,   150,   149,
     149,   143,   150,   149,   149,   119,   179,   652,   667,   373,
    1183,    -1,    -1,    -1,    -1,   530,    -1,    -1,   197,   185,
      -1,   221,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   550,   203,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   209,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   215,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   579
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,     3,   146,   147,   152,   153,   154,   155,   148,     0,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   154,   156,   157,   158,   159,   163,   175,
     195,   206,   213,   220,   235,   249,   267,   279,   286,   293,
     301,   308,   325,   335,   341,   349,   376,   394,   401,   143,
     425,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   157,   149,     4,   146,   147,   167,
     168,   169,     5,   146,   147,   160,   161,   162,     5,   146,
     147,   164,   165,   166,     7,   146,   147,   176,   177,   178,
       5,   146,   147,   250,   251,   252,     5,   146,   147,   336,
     337,   338,     5,   146,   147,   309,   310,   311,     5,   146,
     147,   236,   237,   238,     5,   146,   147,   268,   269,   270,
       5,   146,   147,   214,   215,   216,     5,   146,   147,   196,
     197,   198,     5,   146,   147,   207,   208,   209,     5,   146,
     147,   221,   222,   223,     5,   146,   147,   280,   281,   282,
       5,   146,   147,   294,   295,   296,     5,   146,   147,   302,
     303,   304,     5,   146,   147,   326,   327,   328,     5,   146,
     147,   287,   288,   289,     5,   146,   147,   342,   343,   344,
       5,   146,   147,   350,   351,   352,     5,   146,   147,   377,
     378,   379,     5,   146,   147,   402,   403,   404,     5,   146,
     147,   395,   396,   397,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   170,   168,   143,   161,   143,   165,
     143,   177,   143,   251,   143,   337,   143,   310,   143,   237,
     143,   269,   143,   215,   143,   197,   143,   208,   143,   222,
     143,   281,   143,   295,   143,   303,   143,   327,   143,   288,
     143,   343,   143,   351,   143,   378,   143,   403,   143,   396,
     148,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   143,     6,   167,     4,     6,   146,   147,
     171,   172,   173,     8,    13,    14,    15,    16,    17,   146,
     147,   179,   180,   181,   182,   183,   184,   185,     4,     6,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,   146,   147,   253,   254,   255,   256,   257,   258,
     259,   260,   261,   262,   263,   264,   265,   266,   416,     6,
     142,   146,   147,   339,   340,   416,   417,     6,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,   146,
     147,   312,   313,   314,   315,   316,   317,   318,   319,   320,
     321,   322,   323,   324,   416,     6,    34,    35,    36,    37,
      38,    39,    40,    41,   146,   147,   239,   240,   241,   242,
     243,   244,   245,   246,   247,   248,   416,     6,     9,    10,
      11,    12,   146,   147,   271,   272,   273,   276,   277,   278,
     416,     6,    24,   146,   147,   217,   218,   219,   416,     6,
      18,    19,    20,    21,    22,   146,   147,   199,   200,   201,
     202,   203,   204,   205,   416,     6,    23,   146,   147,   210,
     211,   212,   416,     6,    25,    26,    27,    28,    29,    30,
      31,    32,    33,   146,   147,   224,   225,   226,   227,   228,
     229,   230,   231,   232,   233,   234,   416,     6,    54,   146,
     147,   283,   284,   285,   416,     6,    56,    57,   146,   147,
     297,   298,   299,   300,   416,     6,    58,   146,   147,   305,
     306,   307,   416,     6,    70,    71,    72,    73,   146,   147,
     329,   330,   331,   332,   333,   334,   416,     6,    55,   146,
     147,   290,   291,   292,   416,     6,    74,    75,   146,   147,
     345,   346,   347,   348,   416,     6,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,   146,   147,   353,
     354,   355,   356,   357,   358,   359,   360,   361,   362,   363,
     364,   365,   366,   367,   368,   369,   370,   371,   372,   373,
     374,   375,   416,     6,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   146,   147,   368,   369,
     370,   371,   372,   373,   374,   375,   380,   381,   382,   383,
     384,   385,   386,   387,   388,   389,   390,   391,   392,   393,
     416,     6,   110,   111,   112,   113,   114,   115,   116,   117,
     146,   147,   405,   406,   407,   408,   409,   410,   411,   412,
     413,   414,   416,     6,   109,   146,   147,   398,   399,   400,
     416,   149,   149,     6,   170,   149,     6,   172,   149,   148,
     148,   148,   148,   148,     8,   180,   143,   149,   148,   148,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   148,
       6,   254,   149,   149,     6,   340,   149,   148,   148,   148,
     148,   148,   148,   148,   148,   148,   148,   148,     6,   313,
     149,   148,   148,   148,   148,   148,   148,   148,   148,     6,
     240,   149,   148,   148,   148,   148,     6,   272,   149,   148,
       6,   218,   149,   148,   148,   148,   148,   148,     6,   200,
     149,   148,     6,   211,   149,   148,   148,   148,   148,   148,
     148,   148,   148,   148,     6,   225,   149,   148,     6,   284,
     149,   148,   148,     6,   298,   149,   148,     6,   306,   149,
     148,   148,   148,   148,     6,   330,   149,   148,     6,   291,
     149,   148,   148,     6,   346,   149,   148,   148,   148,   148,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   148,
     148,   148,   148,   148,   148,   148,   148,     6,   354,   149,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   148,
     148,   148,     6,   381,   149,   148,   148,   148,   148,   148,
     148,   148,   148,     6,   406,   149,   148,     6,   399,   149,
     148,   149,   143,   143,   143,   143,   143,   190,   149,   150,
     425,   143,   143,   143,   143,   143,   143,   143,   143,   143,
     143,   143,   149,   149,   143,   421,   421,   421,   143,   423,
     423,   423,   423,   423,   423,   421,   143,   149,   421,   421,
     143,   425,   421,   149,   418,   421,   149,   418,   149,   418,
     149,   144,   150,   421,   425,   143,   149,   143,   150,   149,
     421,   421,   143,   144,   143,   149,   144,   149,   143,   143,
     425,   425,   143,   421,   425,   143,   143,   149,   425,   149,
     422,   423,   424,   425,   149,   144,   149,   421,   425,   143,
     424,   149,   144,   149,   143,   144,   149,   143,   143,   143,
     421,   143,   143,   421,   418,   418,   421,   421,   421,   423,
     421,   425,   421,   421,   421,   421,   425,   143,   149,   143,
     143,   143,   143,   145,   143,   421,   421,   143,   143,   421,
     425,   149,   143,   143,   150,   143,   143,   425,   425,   143,
     143,   149,   425,   149,   143,   149,   149,   149,   150,   150,
     170,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   143,   149,   149,   149,   150,   150,   150,
     150,   150,   150,   149,   149,   149,   149,   149,   149,   149,
     149,   150,   149,   149,   150,   421,   149,   149,   150,   150,
     421,   149,   149,   150,   150,   150,   150,   149,   149,   149,
     150,   143,   149,   150,   149,   149,   150,   149,   150,   149,
     150,   149,   150,   149,   150,   149,   149,   149,   149,   150,
     149,   150,   149,   149,   149,   150,   149,   149,   150,   149,
     149,   149,   150,   150,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   150,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   149,   150,   150,   150,   144,   415,
     149,   149,   150,   150,   149,   149,   150,   150,   149,   150,
     174,   421,   143,   191,   148,   421,   421,   421,   421,   421,
     421,   143,   419,   420,   421,   425,   421,   150,   418,   421,
     149,   150,   421,   143,   143,   425,   143,   150,   149,   143,
     421,   418,   423,   425,   424,   143,   421,   421,   421,   421,
     421,   143,   144,   425,   415,   150,   150,   143,   425,   144,
     143,   143,   149,   150,   150,   150,   143,   149,   149,   149,
     150,   150,   150,   150,   150,   143,   149,   149,   150,   143,
     150,   150,   149,   150,   150,   425,   150,   149,   150,   149,
     149,   150,   150,   150,   150,   149,   149,   149,   150,   150,
     150,   143,   144,   425,   150,   149,   150,   150,   143,   421,
     143,   192,   149,   423,   423,   423,   420,   143,   150,   143,
     150,   421,   421,   144,   425,   149,   425,   421,   143,   424,
     421,   421,   421,   425,   425,   150,   150,   425,   144,   143,
     150,   150,   149,   149,   149,   150,   421,   150,   425,   149,
     150,   149,   149,   149,   150,   150,   149,   149,   149,   150,
     150,   150,   143,   424,   149,   150,   149,   150,   186,   421,
     143,   193,   421,   150,   425,   149,   421,   421,   421,   421,
     415,   424,   149,   425,   143,   150,   143,   150,   150,   144,
     149,   150,   149,   150,   150,   149,   150,   149,   149,   149,
     143,   187,   193,   144,   150,   143,   143,   425,   150,   149,
     150,   150,   144,   150,   149,   150,   150,   144,   188,   143,
     194,   144,   150,   421,   149,   150,   421,   421,   150,   149,
     150,   144,   150,   149,   149,   150,   150,   189,   425,   144,
     149,   150,   274,   275,   421,   149,   421,   421,   149,   150,
     149,   274,   425,   149,   275,   149,   149,   150,   425,   149,
     421,   149
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   151,   152,   152,   153,   153,   154,   154,   154,   155,
     156,   156,   157,   157,   157,   157,   157,   157,   157,   157,
     157,   157,   157,   157,   157,   157,   157,   157,   157,   157,
     157,   157,   157,   157,   157,   158,   158,   159,   159,   160,
     160,   161,   161,   161,   162,   162,   163,   163,   164,   164,
     165,   165,   165,   166,   166,   167,   167,   168,   168,   168,
     169,   170,   170,   170,   170,   170,   170,   170,   170,   170,
     170,   170,   170,   170,   170,   170,   170,   170,   170,   170,
     170,   171,   171,   172,   172,   172,   173,   173,   174,   174,
     175,   175,   176,   176,   177,   177,   177,   178,   178,   179,
     179,   180,   180,   180,   180,   180,   180,   180,   181,   182,
     183,   184,   185,   185,   186,   186,   187,   187,   188,   188,
     189,   189,   189,   190,   190,   191,   191,   192,   192,   193,
     193,   194,   194,   195,   195,   196,   196,   197,   197,   197,
     198,   198,   199,   199,   200,   200,   200,   200,   200,   200,
     200,   200,   201,   202,   203,   204,   205,   206,   206,   207,
     207,   208,   208,   208,   209,   209,   210,   210,   211,   211,
     211,   211,   212,   213,   213,   214,   214,   215,   215,   215,
     216,   216,   217,   217,   218,   218,   218,   218,   219,   219,
     219,   219,   220,   220,   221,   221,   222,   222,   222,   223,
     223,   224,   224,   225,   225,   225,   225,   225,   225,   225,
     225,   225,   225,   225,   225,   226,   227,   228,   229,   229,
     230,   231,   231,   232,   233,   233,   233,   234,   235,   235,
     236,   236,   237,   237,   237,   238,   238,   239,   239,   240,
     240,   240,   240,   240,   240,   240,   240,   240,   240,   240,
     241,   242,   243,   244,   245,   246,   246,   247,   247,   248,
     248,   249,   249,   250,   250,   251,   251,   251,   252,   252,
     253,   253,   254,   254,   254,   254,   254,   254,   254,   254,
     254,   254,   254,   254,   254,   254,   254,   255,   256,   257,
     258,   259,   260,   261,   262,   263,   264,   265,   266,   267,
     267,   268,   268,   269,   269,   269,   270,   270,   271,   271,
     272,   272,   272,   272,   272,   272,   272,   273,   273,   273,
     273,   274,   274,   275,   276,   277,   278,   279,   279,   280,
     280,   281,   281,   281,   282,   282,   283,   283,   284,   284,
     284,   284,   285,   286,   286,   287,   287,   288,   288,   288,
     289,   289,   290,   290,   291,   291,   291,   291,   292,   292,
     292,   292,   292,   292,   293,   293,   294,   294,   295,   295,
     295,   296,   296,   297,   297,   298,   298,   298,   298,   298,
     299,   300,   301,   301,   302,   302,   303,   303,   303,   304,
     304,   305,   305,   306,   306,   306,   306,   307,   307,   308,
     308,   309,   309,   310,   310,   310,   311,   311,   312,   312,
     313,   313,   313,   313,   313,   313,   313,   313,   313,   313,
     313,   313,   313,   313,   314,   315,   316,   317,   318,   319,
     320,   321,   322,   323,   324,   325,   325,   326,   326,   327,
     327,   327,   328,   328,   329,   329,   330,   330,   330,   330,
     330,   330,   330,   331,   332,   333,   334,   335,   335,   336,
     336,   337,   337,   337,   338,   338,   339,   339,   340,   340,
     340,   340,   341,   341,   342,   342,   343,   343,   343,   344,
     344,   345,   345,   346,   346,   346,   346,   346,   347,   348,
     349,   349,   350,   350,   351,   351,   351,   352,   352,   353,
     353,   354,   354,   354,   354,   354,   354,   354,   354,   354,
     354,   354,   354,   354,   354,   354,   354,   354,   354,   354,
     354,   354,   354,   354,   354,   355,   356,   357,   358,   359,
     360,   361,   362,   363,   364,   365,   366,   367,   368,   369,
     370,   371,   372,   373,   374,   375,   376,   376,   377,   377,
     378,   378,   378,   379,   379,   380,   380,   381,   381,   381,
     381,   381,   381,   381,   381,   381,   381,   381,   381,   381,
     381,   381,   381,   381,   381,   381,   381,   381,   381,   381,
     382,   382,   383,   384,   385,   386,   387,   388,   389,   390,
     391,   392,   393,   394,   394,   395,   395,   396,   396,   396,
     397,   397,   398,   398,   399,   399,   399,   399,   400,   401,
     401,   402,   402,   403,   403,   403,   404,   404,   405,   405,
     406,   406,   406,   406,   406,   406,   406,   406,   406,   406,
     406,   407,   408,   408,   409,   410,   411,   411,   412,   413,
     414,   414,   415,   415,   416,   417,   418,   418,   419,   419,
     420,   420,   421,   422,   422,   423,   424,   424,   425
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     1,     2,     1,     1,     1,     1,     4,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     2,     3,     2,     2,
       1,     1,     1,     1,     6,     5,     3,     2,     2,     1,
       1,     1,     1,     6,     5,     2,     1,     1,     1,     1,
       5,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     1,     1,     1,     6,     5,     3,     2,
       3,     2,     2,     1,     1,     1,     1,     6,     5,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     4,
       4,    16,    14,    12,     0,     1,     0,     1,     0,     1,
       0,     1,     3,     0,     1,     0,     1,     0,     1,     0,
       2,     0,     1,     3,     2,     2,     1,     1,     1,     1,
       6,     5,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     4,     6,     4,     8,    16,     3,     2,     2,
       1,     1,     1,     1,     6,     5,     2,     1,     1,     1,
       1,     1,     8,     3,     2,     2,     1,     1,     1,     1,
       6,     5,     2,     1,     1,     1,     1,     1,     5,     6,
      10,     9,     3,     2,     2,     1,     1,     1,     1,     6,
       5,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     4,     4,     4,     8,     7,
       5,     4,     8,     4,     4,     6,    10,     4,     3,     2,
       2,     1,     1,     1,     1,     6,     5,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     4,     4,     4,     4,     4,     3,     4,     3,     4,
       3,     3,     2,     2,     1,     1,     1,     1,     6,     5,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     4,     4,     4,
       4,     4,     4,     4,     4,     4,     4,     4,     4,     3,
       2,     2,     1,     1,     1,     1,     6,     5,     2,     1,
       1,     1,     1,     1,     1,     1,     1,    16,    17,    15,
      16,     2,     1,     2,     4,     4,     6,     3,     2,     2,
       1,     1,     1,     1,     6,     5,     2,     1,     1,     1,
       1,     1,     6,     3,     2,     2,     1,     1,     1,     1,
       6,     5,     2,     1,     1,     1,     1,     1,    16,    12,
      14,    13,    14,    15,     3,     2,     2,     1,     1,     1,
       1,     6,     5,     2,     1,     1,     1,     1,     1,     1,
       4,     4,     3,     2,     2,     1,     1,     1,     1,     6,
       5,     2,     1,     1,     1,     1,     1,     6,     4,     3,
       2,     2,     1,     1,     1,     1,     6,     5,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     4,     4,     4,     6,     6,
       6,     8,     8,     8,     4,     3,     2,     2,     1,     1,
       1,     1,     6,     5,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     4,     4,     4,     4,     3,     2,     2,
       1,     1,     1,     1,     6,     5,     2,     1,     1,     1,
       1,     1,     3,     2,     2,     1,     1,     1,     1,     6,
       5,     2,     1,     1,     1,     1,     1,     1,     4,     8,
       3,     2,     2,     1,     1,     1,     1,     6,     5,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     4,     4,     4,     8,     4,
       4,     8,     4,     4,     4,     6,     6,     4,     4,     4,
       4,     4,     4,     4,     4,     4,     3,     2,     2,     1,
       1,     1,     1,     6,     5,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     6,     4,     4,     4,     4,     4,     4,     4,     4,
       4,     4,    18,     3,     2,     2,     1,     1,     1,     1,
       6,     5,     2,     1,     1,     1,     1,     1,    10,     3,
       2,     2,     1,     1,     1,     1,     6,     5,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,    10,    10,     9,     4,     4,    10,     8,     6,     4,
       8,     4,     5,     3,     7,     2,     3,     1,     3,     1,
       1,     1,     2,     3,     1,     1,     3,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2:
#line 336 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {vex_ptr=make_vex((yyvsp[-1].llptr),(yyvsp[0].llptr));}
#line 2586 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 3:
#line 337 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {vex_ptr=make_vex((yyvsp[0].llptr),NULL);}
#line 2592 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 4:
#line 339 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 2598 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 5:
#line 340 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 2604 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 6:
#line 342 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_VEX_REV,(yyvsp[0].dvptr));}
#line 2610 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 7:
#line 343 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 2616 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 8:
#line 344 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 2622 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 9:
#line 348 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 2628 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 10:
#line 353 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].blptr));}
#line 2634 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 11:
#line 354 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].blptr));}
#line 2640 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 12:
#line 356 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_GLOBAL,(yyvsp[0].llptr));}
#line 2646 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 13:
#line 357 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_STATION,(yyvsp[0].llptr));}
#line 2652 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 14:
#line 358 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_MODE,(yyvsp[0].llptr));}
#line 2658 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 15:
#line 359 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_FREQ,(yyvsp[0].llptr));}
#line 2664 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 16:
#line 360 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_SCHED,(yyvsp[0].llptr));}
#line 2670 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 17:
#line 361 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_ANTENNA,(yyvsp[0].llptr));}
#line 2676 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 18:
#line 362 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_BBC,(yyvsp[0].llptr));}
#line 2682 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 19:
#line 363 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_CLOCK,(yyvsp[0].llptr));}
#line 2688 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 20:
#line 364 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_DAS,(yyvsp[0].llptr));}
#line 2694 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 21:
#line 365 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_EOP,(yyvsp[0].llptr));}
#line 2700 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 22:
#line 366 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_EXPER,(yyvsp[0].llptr));}
#line 2706 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 23:
#line 367 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_HEAD_POS,(yyvsp[0].llptr));}
#line 2712 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 24:
#line 368 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_IF,(yyvsp[0].llptr));}
#line 2718 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 25:
#line 369 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_PASS_ORDER,(yyvsp[0].llptr));}
#line 2724 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 26:
#line 370 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_PHASE_CAL_DETECT,(yyvsp[0].llptr));}
#line 2730 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 27:
#line 371 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_PROCEDURES,(yyvsp[0].llptr));}
#line 2736 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 28:
#line 372 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_ROLL,(yyvsp[0].llptr));}
#line 2742 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 29:
#line 374 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.blptr)=make_block(B_SCHEDULING_PARAMS,(yyvsp[0].llptr));}
#line 2748 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 30:
#line 375 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_SEFD,(yyvsp[0].llptr));}
#line 2754 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 31:
#line 376 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_SITE,(yyvsp[0].llptr));}
#line 2760 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 32:
#line 377 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_SOURCE,(yyvsp[0].llptr));}
#line 2766 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 33:
#line 378 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_TAPELOG_OBS,(yyvsp[0].llptr));}
#line 2772 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 34:
#line 379 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.blptr)=make_block(B_TRACKS,(yyvsp[0].llptr));}
#line 2778 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 35:
#line 383 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 2784 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 36:
#line 384 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 2790 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 37:
#line 388 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=(yyvsp[0].llptr);}
#line 2796 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 38:
#line 389 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=NULL;}
#line 2802 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 39:
#line 391 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 2808 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 40:
#line 392 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 2814 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 41:
#line 394 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 2820 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 42:
#line 395 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 2826 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 43:
#line 396 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 2832 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 44:
#line 398 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 2838 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 45:
#line 399 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 2844 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 46:
#line 403 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 2850 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 47:
#line 404 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 2856 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 48:
#line 406 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 2862 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 49:
#line 407 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 2868 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 50:
#line 409 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 2874 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 51:
#line 410 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 2880 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 52:
#line 411 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 2886 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 53:
#line 413 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 2892 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 54:
#line 415 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 2898 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 55:
#line 419 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 2904 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 56:
#line 420 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 2910 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 57:
#line 422 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].qrptr));}
#line 2916 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 58:
#line 423 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 2922 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 59:
#line 424 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 2928 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 60:
#line 426 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.qrptr)=make_qref((yyvsp[-3].ival),(yyvsp[-1].sval),NULL);}
#line 2934 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 61:
#line 428 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_EXPER;}
#line 2940 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 62:
#line 429 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_SCHEDULING_PARAMS;}
#line 2946 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 63:
#line 430 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_PROCEDURES;}
#line 2952 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 64:
#line 431 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_EOP;}
#line 2958 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 65:
#line 432 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_FREQ;}
#line 2964 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 66:
#line 433 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_ANTENNA;}
#line 2970 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 67:
#line 434 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_BBC;}
#line 2976 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 68:
#line 435 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_CLOCK;}
#line 2982 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 69:
#line 436 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_CORR;}
#line 2988 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 70:
#line 437 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_DAS;}
#line 2994 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 71:
#line 438 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_HEAD_POS;}
#line 3000 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 72:
#line 439 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_PASS_ORDER;}
#line 3006 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 73:
#line 440 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_PHASE_CAL_DETECT;}
#line 3012 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 74:
#line 441 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_ROLL;}
#line 3018 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 75:
#line 442 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_IF;}
#line 3024 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 76:
#line 443 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_SEFD;}
#line 3030 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 77:
#line 444 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_SITE;}
#line 3036 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 78:
#line 445 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_SOURCE;}
#line 3042 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 79:
#line 446 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_TRACKS;}
#line 3048 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 80:
#line 447 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.ival)=B_TAPELOG_OBS;}
#line 3054 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 81:
#line 449 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3060 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 82:
#line 450 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3066 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 83:
#line 452 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].qrptr));}
#line 3072 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 84:
#line 453 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3078 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 85:
#line 454 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3084 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 86:
#line 456 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                  {(yyval.qrptr)=make_qref((yyvsp[-4].ival),(yyvsp[-2].sval),(yyvsp[-1].llptr));}
#line 3090 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 87:
#line 457 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.qrptr)=make_qref((yyvsp[-3].ival),(yyvsp[-1].sval),NULL);}
#line 3096 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 88:
#line 459 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-2].llptr),(yyvsp[0].sval));}
#line 3102 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 89:
#line 460 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].sval));}
#line 3108 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 90:
#line 464 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 3114 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 91:
#line 465 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 3120 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 92:
#line 467 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3126 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 93:
#line 468 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3132 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 94:
#line 470 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_SCAN,(yyvsp[0].dfptr));}
#line 3138 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 95:
#line 471 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3144 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 96:
#line 472 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3150 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 97:
#line 475 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 3156 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 98:
#line 476 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 3162 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 99:
#line 478 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3168 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 100:
#line 479 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3174 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 101:
#line 481 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_START,(yyvsp[0].sval));}
#line 3180 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 102:
#line 482 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_MODE,(yyvsp[0].sval));}
#line 3186 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 103:
#line 483 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_SOURCE,(yyvsp[0].sval));}
#line 3192 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 104:
#line 484 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_STATION,(yyvsp[0].snptr));}
#line 3198 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 105:
#line 485 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DATA_TRANSFER,(yyvsp[0].dtptr));}
#line 3204 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 106:
#line 486 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3210 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 107:
#line 487 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3216 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 108:
#line 489 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.sval)=(yyvsp[-1].sval);}
#line 3222 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 109:
#line 491 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.sval)=(yyvsp[-1].sval);}
#line 3228 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 110:
#line 493 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.sval)=(yyvsp[-1].sval);}
#line 3234 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 111:
#line 502 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.snptr)=make_station((yyvsp[-13].sval),(yyvsp[-11].dvptr),(yyvsp[-9].dvptr),(yyvsp[-7].dvptr),(yyvsp[-5].sval),(yyvsp[-3].sval),(yyvsp[-1].llptr));}
#line 3240 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 112:
#line 510 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.dtptr)=make_data_transfer((yyvsp[-11].sval),(yyvsp[-9].sval),(yyvsp[-7].sval),(yyvsp[-5].dvptr),(yyvsp[-3].dvptr),(yyvsp[-1].sval));}
#line 3246 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 113:
#line 516 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.dtptr)=make_data_transfer((yyvsp[-9].sval),(yyvsp[-7].sval),(yyvsp[-5].sval),(yyvsp[-3].dvptr),(yyvsp[-1].dvptr),NULL);}
#line 3252 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 114:
#line 518 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=NULL;}
#line 3258 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 115:
#line 519 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=(yyvsp[0].dvptr);}
#line 3264 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 116:
#line 521 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=NULL;}
#line 3270 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 117:
#line 522 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[0].sval);}
#line 3276 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 118:
#line 524 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=NULL;}
#line 3282 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 119:
#line 525 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[0].sval);}
#line 3288 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 120:
#line 527 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=NULL;}
#line 3294 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 121:
#line 528 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].dvptr));}
#line 3300 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 122:
#line 529 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(add_list(NULL,(yyvsp[-2].dvptr)),(yyvsp[0].dvptr));}
#line 3306 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 123:
#line 531 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=NULL;}
#line 3312 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 124:
#line 532 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[0].sval);}
#line 3318 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 125:
#line 534 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=NULL;}
#line 3324 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 126:
#line 535 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[0].sval);}
#line 3330 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 127:
#line 537 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=NULL;}
#line 3336 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 128:
#line 538 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[0].sval);}
#line 3342 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 129:
#line 540 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=make_dvalue(NULL,NULL);}
#line 3348 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 130:
#line 541 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=make_dvalue((yyvsp[-1].sval),(yyvsp[0].sval));}
#line 3354 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 131:
#line 543 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=NULL;}
#line 3360 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 132:
#line 544 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[0].sval);}
#line 3366 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 133:
#line 549 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=(yyvsp[0].llptr);}
#line 3372 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 134:
#line 550 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=NULL;}
#line 3378 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 135:
#line 552 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3384 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 136:
#line 553 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3390 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 137:
#line 555 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 3396 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 138:
#line 556 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3402 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 139:
#line 557 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3408 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 140:
#line 560 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 3414 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 141:
#line 561 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 3420 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 142:
#line 563 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3426 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 143:
#line 564 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3432 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 144:
#line 566 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ANTENNA_DIAM,(yyvsp[0].dvptr));}
#line 3438 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 145:
#line 567 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_AXIS_TYPE,(yyvsp[0].atptr));}
#line 3444 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 146:
#line 568 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_AXIS_OFFSET,(yyvsp[0].dvptr));}
#line 3450 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 147:
#line 569 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ANTENNA_MOTION,(yyvsp[0].amptr));}
#line 3456 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 148:
#line 570 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_POINTING_SECTOR,(yyvsp[0].psptr));}
#line 3462 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 149:
#line 571 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 3468 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 150:
#line 572 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3474 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 151:
#line 573 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3480 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 152:
#line 575 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 3486 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 153:
#line 578 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.atptr)=make_axis_type((yyvsp[-3].sval),(yyvsp[-1].sval));}
#line 3492 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 154:
#line 580 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 3498 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 155:
#line 585 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.amptr)=make_antenna_motion((yyvsp[-5].sval),(yyvsp[-3].dvptr),(yyvsp[-1].dvptr));}
#line 3504 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 156:
#line 594 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                        {(yyval.psptr)=make_pointing_sector((yyvsp[-13].sval),(yyvsp[-11].sval),(yyvsp[-9].dvptr),(yyvsp[-7].dvptr),(yyvsp[-5].sval),(yyvsp[-3].dvptr),(yyvsp[-1].dvptr));}
#line 3510 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 157:
#line 598 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 3516 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 158:
#line 599 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 3522 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 159:
#line 601 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3528 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 160:
#line 602 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3534 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 161:
#line 604 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 3540 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 162:
#line 605 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3546 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 163:
#line 606 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3552 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 164:
#line 608 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 3558 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 165:
#line 610 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 3564 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 166:
#line 612 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3570 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 167:
#line 613 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3576 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 168:
#line 615 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_BBC_ASSIGN,(yyvsp[0].baptr));}
#line 3582 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 169:
#line 616 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 3588 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 170:
#line 617 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3594 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 171:
#line 618 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3600 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 172:
#line 621 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.baptr)=make_bbc_assign((yyvsp[-5].sval),(yyvsp[-3].dvptr),(yyvsp[-1].sval));}
#line 3606 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 173:
#line 625 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 3612 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 174:
#line 626 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 3618 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 175:
#line 628 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3624 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 176:
#line 629 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3630 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 177:
#line 631 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 3636 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 178:
#line 632 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3642 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 179:
#line 633 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3648 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 180:
#line 636 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 3654 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 181:
#line 638 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 3660 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 182:
#line 640 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3666 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 183:
#line 641 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3672 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 184:
#line 643 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_CLOCK_EARLY,(yyvsp[0].ceptr));}
#line 3678 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 185:
#line 644 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 3684 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 186:
#line 645 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3690 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 187:
#line 646 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3696 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 188:
#line 649 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.ceptr)=make_clock_early(NULL,(yyvsp[-1].dvptr),NULL,NULL);}
#line 3702 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 189:
#line 651 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.ceptr)=make_clock_early((yyvsp[-3].sval),(yyvsp[-1].dvptr),NULL,NULL);}
#line 3708 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 190:
#line 653 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.ceptr)=make_clock_early((yyvsp[-7].sval),(yyvsp[-5].dvptr),(yyvsp[-3].sval),(yyvsp[-1].dvptr));}
#line 3714 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 191:
#line 655 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.ceptr)=make_clock_early(NULL,(yyvsp[-5].dvptr),(yyvsp[-3].sval),(yyvsp[-1].dvptr));}
#line 3720 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 192:
#line 659 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 3726 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 193:
#line 660 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 3732 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 194:
#line 662 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3738 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 195:
#line 663 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3744 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 196:
#line 665 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 3750 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 197:
#line 666 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3756 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 198:
#line 667 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3762 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 199:
#line 669 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 3768 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 200:
#line 671 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 3774 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 201:
#line 673 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3780 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 202:
#line 674 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3786 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 203:
#line 676 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                      {(yyval.lwptr)=make_lowl(T_RECORD_TRANSPORT_TYPE,(yyvsp[0].sval));}
#line 3792 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 204:
#line 677 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ELECTRONICS_RACK_TYPE,(yyvsp[0].sval));}
#line 3798 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 205:
#line 678 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_NUMBER_DRIVES,(yyvsp[0].dvptr));}
#line 3804 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 206:
#line 679 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_HEADSTACK,(yyvsp[0].hsptr));}
#line 3810 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 207:
#line 680 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_RECORD_DENSITY,(yyvsp[0].dvptr));}
#line 3816 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 208:
#line 681 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_TAPE_LENGTH,(yyvsp[0].tlptr));}
#line 3822 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 209:
#line 683 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_RECORDING_SYSTEM_ID,(yyvsp[0].dvptr));}
#line 3828 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 210:
#line 684 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_TAPE_MOTION,(yyvsp[0].tmptr));}
#line 3834 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 211:
#line 685 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_TAPE_CONTROL,(yyvsp[0].sval));}
#line 3840 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 212:
#line 686 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 3846 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 213:
#line 687 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3852 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 214:
#line 688 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3858 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 215:
#line 690 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 3864 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 216:
#line 692 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 3870 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 217:
#line 694 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 3876 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 218:
#line 697 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.hsptr)=make_headstack((yyvsp[-5].dvptr),(yyvsp[-3].sval),(yyvsp[-1].dvptr));}
#line 3882 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 219:
#line 699 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.hsptr)=make_headstack((yyvsp[-4].dvptr),NULL,(yyvsp[-1].dvptr));}
#line 3888 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 220:
#line 702 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=make_dvalue((yyvsp[-2].sval),(yyvsp[-1].sval));}
#line 3894 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 221:
#line 705 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.tlptr)=make_tape_length((yyvsp[-1].dvptr),NULL,NULL);}
#line 3900 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 222:
#line 707 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.tlptr)=make_tape_length((yyvsp[-5].dvptr),(yyvsp[-3].sval),(yyvsp[-1].dvptr));}
#line 3906 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 223:
#line 709 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 3912 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 224:
#line 712 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.tmptr)=make_tape_motion((yyvsp[-1].sval),NULL,NULL,NULL);}
#line 3918 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 225:
#line 714 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.tmptr)=make_tape_motion((yyvsp[-3].sval),(yyvsp[-1].dvptr),NULL,NULL);}
#line 3924 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 226:
#line 717 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.tmptr)=make_tape_motion((yyvsp[-7].sval),(yyvsp[-5].dvptr),(yyvsp[-3].dvptr),(yyvsp[-1].dvptr));}
#line 3930 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 227:
#line 719 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                              {(yyval.sval)=(yyvsp[-1].sval);}
#line 3936 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 228:
#line 723 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 3942 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 229:
#line 724 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 3948 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 230:
#line 726 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3954 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 231:
#line 727 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 3960 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 232:
#line 729 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 3966 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 233:
#line 730 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 3972 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 234:
#line 731 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 3978 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 235:
#line 733 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 3984 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 236:
#line 735 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 3990 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 237:
#line 737 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 3996 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 238:
#line 738 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4002 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 239:
#line 740 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_TAI_UTC,(yyvsp[0].dvptr));}
#line 4008 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 240:
#line 741 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_A1_TAI,(yyvsp[0].dvptr));}
#line 4014 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 241:
#line 742 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_EOP_REF_EPOCH,(yyvsp[0].sval));}
#line 4020 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 242:
#line 743 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_NUM_EOP_POINTS,(yyvsp[0].dvptr));}
#line 4026 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 243:
#line 744 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_EOP_INTERVAL,(yyvsp[0].dvptr));}
#line 4032 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 244:
#line 745 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_UT1_UTC,(yyvsp[0].llptr));}
#line 4038 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 245:
#line 746 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_X_WOBBLE,(yyvsp[0].llptr));}
#line 4044 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 246:
#line 747 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_Y_WOBBLE,(yyvsp[0].llptr));}
#line 4050 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 247:
#line 748 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 4056 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 248:
#line 749 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4062 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 249:
#line 750 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4068 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 250:
#line 752 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 4074 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 251:
#line 754 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 4080 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 252:
#line 756 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4086 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 253:
#line 758 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 4092 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 254:
#line 760 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 4098 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 255:
#line 762 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=(yyvsp[-1].llptr);}
#line 4104 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 256:
#line 763 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=NULL;}
#line 4110 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 257:
#line 765 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=(yyvsp[-1].llptr);}
#line 4116 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 258:
#line 766 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=NULL;}
#line 4122 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 259:
#line 768 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=(yyvsp[-1].llptr);}
#line 4128 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 260:
#line 769 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=NULL;}
#line 4134 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 261:
#line 773 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 4140 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 262:
#line 774 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 4146 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 263:
#line 776 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4152 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 264:
#line 777 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4158 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 265:
#line 779 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 4164 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 266:
#line 780 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4170 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 267:
#line 781 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4176 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 268:
#line 784 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 4182 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 269:
#line 785 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 4188 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 270:
#line 787 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4194 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 271:
#line 788 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4200 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 272:
#line 790 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_EXPER_NUM,(yyvsp[0].dvptr));}
#line 4206 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 273:
#line 791 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_EXPER_NAME,(yyvsp[0].sval));}
#line 4212 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 274:
#line 792 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_EXPER_DESCRIPTION,(yyvsp[0].sval));}
#line 4218 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 275:
#line 794 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_EXPER_NOMINAL_START,(yyvsp[0].sval));}
#line 4224 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 276:
#line 796 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_EXPER_NOMINAL_STOP,(yyvsp[0].sval));}
#line 4230 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 277:
#line 797 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_PI_NAME,(yyvsp[0].sval));}
#line 4236 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 278:
#line 798 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_PI_EMAIL,(yyvsp[0].sval));}
#line 4242 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 279:
#line 799 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_CONTACT_NAME,(yyvsp[0].sval));}
#line 4248 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 280:
#line 800 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_CONTACT_EMAIL,(yyvsp[0].sval));}
#line 4254 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 281:
#line 801 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_SCHEDULER_NAME,(yyvsp[0].sval));}
#line 4260 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 282:
#line 802 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_SCHEDULER_EMAIL,(yyvsp[0].sval));}
#line 4266 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 283:
#line 804 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_TARGET_CORRELATOR,(yyvsp[0].sval));}
#line 4272 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 284:
#line 805 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 4278 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 285:
#line 806 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4284 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 286:
#line 807 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4290 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 287:
#line 809 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                          {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 4296 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 288:
#line 811 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4302 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 289:
#line 813 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4308 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 290:
#line 815 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4314 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 291:
#line 817 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4320 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 292:
#line 819 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4326 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 293:
#line 821 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4332 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 294:
#line 823 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4338 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 295:
#line 825 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4344 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 296:
#line 827 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4350 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 297:
#line 829 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4356 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 298:
#line 831 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 4362 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 299:
#line 835 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 4368 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 300:
#line 836 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 4374 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 301:
#line 838 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4380 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 302:
#line 839 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4386 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 303:
#line 841 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 4392 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 304:
#line 842 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4398 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 305:
#line 843 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4404 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 306:
#line 845 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                         {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 4410 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 307:
#line 847 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 4416 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 308:
#line 849 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4422 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 309:
#line 850 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4428 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 310:
#line 852 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_CHAN_DEF,(yyvsp[0].cdptr));}
#line 4434 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 311:
#line 853 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_SAMPLE_RATE,(yyvsp[0].dvptr));}
#line 4440 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 312:
#line 854 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_BITS_PER_SAMPLE,(yyvsp[0].dvptr));}
#line 4446 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 313:
#line 855 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_SWITCHING_CYCLE,(yyvsp[0].scptr));}
#line 4452 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 314:
#line 856 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 4458 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 315:
#line 857 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4464 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 316:
#line 858 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4470 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 317:
#line 867 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.cdptr)=make_chan_def((yyvsp[-13].sval),(yyvsp[-11].dvptr),(yyvsp[-9].sval),(yyvsp[-7].dvptr),(yyvsp[-5].sval),(yyvsp[-3].sval),(yyvsp[-1].sval),NULL);}
#line 4476 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 318:
#line 875 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.cdptr)=make_chan_def((yyvsp[-14].sval),(yyvsp[-12].dvptr),(yyvsp[-10].sval),(yyvsp[-8].dvptr),(yyvsp[-6].sval),(yyvsp[-4].sval),(yyvsp[-2].sval),(yyvsp[-1].llptr));}
#line 4482 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 319:
#line 883 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.cdptr)=make_chan_def(NULL,(yyvsp[-11].dvptr),(yyvsp[-9].sval),(yyvsp[-7].dvptr),(yyvsp[-5].sval),(yyvsp[-3].sval),(yyvsp[-1].sval),NULL);}
#line 4488 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 320:
#line 891 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.cdptr)=make_chan_def(NULL,(yyvsp[-12].dvptr),(yyvsp[-10].sval),(yyvsp[-8].dvptr),(yyvsp[-6].sval),(yyvsp[-4].sval),(yyvsp[-2].sval),(yyvsp[-1].llptr));}
#line 4494 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 321:
#line 893 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].dvptr));}
#line 4500 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 322:
#line 894 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].dvptr));}
#line 4506 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 323:
#line 896 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=(yyvsp[0].dvptr);}
#line 4512 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 324:
#line 898 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 4518 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 325:
#line 900 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 4524 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 326:
#line 903 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.scptr)=make_switching_cycle((yyvsp[-3].sval),(yyvsp[-1].llptr));}
#line 4530 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 327:
#line 907 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=(yyvsp[0].llptr);}
#line 4536 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 328:
#line 908 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=NULL;}
#line 4542 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 329:
#line 910 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4548 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 330:
#line 911 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4554 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 331:
#line 913 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 4560 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 332:
#line 914 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4566 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 333:
#line 915 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4572 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 334:
#line 918 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 4578 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 335:
#line 920 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 4584 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 336:
#line 922 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4590 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 337:
#line 923 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4596 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 338:
#line 925 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_HEADSTACK_POS,(yyvsp[0].hpptr));}
#line 4602 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 339:
#line 926 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 4608 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 340:
#line 927 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4614 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 341:
#line 928 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4620 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 342:
#line 931 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.hpptr)=make_headstack_pos((yyvsp[-3].dvptr),(yyvsp[-1].llptr));}
#line 4626 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 343:
#line 935 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 4632 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 344:
#line 936 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 4638 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 345:
#line 938 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4644 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 346:
#line 939 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4650 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 347:
#line 941 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 4656 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 348:
#line 942 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4662 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 349:
#line 943 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4668 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 350:
#line 945 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 4674 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 351:
#line 947 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 4680 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 352:
#line 949 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4686 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 353:
#line 950 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4692 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 354:
#line 952 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_IF_DEF,(yyvsp[0].ifptr));}
#line 4698 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 355:
#line 953 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 4704 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 356:
#line 954 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4710 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 357:
#line 955 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4716 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 358:
#line 958 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                        {(yyval.ifptr)=make_if_def((yyvsp[-13].sval),(yyvsp[-11].sval),(yyvsp[-9].sval),(yyvsp[-7].dvptr),(yyvsp[-5].sval),(yyvsp[-3].dvptr),(yyvsp[-1].dvptr));}
#line 4722 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 359:
#line 960 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                        {(yyval.ifptr)=make_if_def((yyvsp[-9].sval),(yyvsp[-7].sval),(yyvsp[-5].sval),(yyvsp[-3].dvptr),(yyvsp[-1].sval),NULL,NULL);}
#line 4728 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 360:
#line 962 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                        {(yyval.ifptr)=make_if_def((yyvsp[-11].sval),(yyvsp[-9].sval),(yyvsp[-7].sval),(yyvsp[-5].dvptr),(yyvsp[-3].sval),NULL,NULL);}
#line 4734 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 361:
#line 964 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                        {(yyval.ifptr)=make_if_def((yyvsp[-10].sval),(yyvsp[-8].sval),(yyvsp[-6].sval),(yyvsp[-4].dvptr),(yyvsp[-2].sval),NULL,NULL);}
#line 4740 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 362:
#line 966 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                        {(yyval.ifptr)=make_if_def((yyvsp[-11].sval),(yyvsp[-9].sval),(yyvsp[-7].sval),(yyvsp[-5].dvptr),(yyvsp[-3].sval),(yyvsp[-1].dvptr),NULL);}
#line 4746 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 363:
#line 968 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                        {(yyval.ifptr)=make_if_def((yyvsp[-12].sval),(yyvsp[-10].sval),(yyvsp[-8].sval),(yyvsp[-6].dvptr),(yyvsp[-4].sval),(yyvsp[-2].dvptr),NULL);}
#line 4752 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 364:
#line 972 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.llptr)=(yyvsp[0].llptr);}
#line 4758 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 365:
#line 973 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.llptr)=NULL;}
#line 4764 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 366:
#line 975 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4770 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 367:
#line 977 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4776 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 368:
#line 979 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 4782 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 369:
#line 980 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4788 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 370:
#line 981 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4794 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 371:
#line 984 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 4800 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 372:
#line 986 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 4806 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 373:
#line 989 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4812 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 374:
#line 990 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4818 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 375:
#line 992 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_PASS_ORDER,(yyvsp[0].llptr));}
#line 4824 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 376:
#line 994 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_S2_GROUP_ORDER,(yyvsp[0].llptr));}
#line 4830 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 377:
#line 995 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 4836 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 378:
#line 996 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4842 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 379:
#line 997 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4848 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 380:
#line 999 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                               {(yyval.llptr)=(yyvsp[-1].llptr);}
#line 4854 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 381:
#line 1001 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                    {(yyval.llptr)=(yyvsp[-1].llptr);}
#line 4860 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 382:
#line 1005 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 4866 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 383:
#line 1006 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.llptr)=NULL;}
#line 4872 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 384:
#line 1009 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4878 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 385:
#line 1010 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4884 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 386:
#line 1012 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 4890 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 387:
#line 1013 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4896 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 388:
#line 1014 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4902 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 389:
#line 1017 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 4908 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 390:
#line 1018 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 4914 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 391:
#line 1021 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4920 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 392:
#line 1022 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4926 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 393:
#line 1024 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                         {(yyval.lwptr)=make_lowl(T_PHASE_CAL_DETECT,(yyvsp[0].pdptr));}
#line 4932 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 394:
#line 1025 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 4938 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 395:
#line 1026 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4944 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 396:
#line 1027 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 4950 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 397:
#line 1030 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.pdptr)=make_phase_cal_detect((yyvsp[-3].sval),(yyvsp[-1].llptr));}
#line 4956 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 398:
#line 1032 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.pdptr)=make_phase_cal_detect((yyvsp[-1].sval),NULL);}
#line 4962 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 399:
#line 1036 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.llptr)=(yyvsp[0].llptr);}
#line 4968 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 400:
#line 1037 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.llptr)=NULL;}
#line 4974 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 401:
#line 1040 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 4980 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 402:
#line 1041 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 4986 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 403:
#line 1043 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 4992 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 404:
#line 1044 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 4998 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 405:
#line 1045 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 5004 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 406:
#line 1048 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 5010 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 407:
#line 1050 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 5016 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 408:
#line 1053 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 5022 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 409:
#line 1054 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 5028 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 410:
#line 1057 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                 {(yyval.lwptr)=make_lowl(T_TAPE_CHANGE,(yyvsp[0].dvptr));}
#line 5034 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 411:
#line 1059 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_HEADSTACK_MOTION,(yyvsp[0].dvptr));}
#line 5040 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 412:
#line 1061 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_NEW_SOURCE_COMMAND,(yyvsp[0].dvptr));}
#line 5046 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 413:
#line 1063 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_NEW_TAPE_SETUP,(yyvsp[0].dvptr));}
#line 5052 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 414:
#line 1065 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_SETUP_ALWAYS,(yyvsp[0].saptr));}
#line 5058 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 415:
#line 1067 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_PARITY_CHECK,(yyvsp[0].pcptr));}
#line 5064 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 416:
#line 1069 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_TAPE_PREPASS,(yyvsp[0].tpptr));}
#line 5070 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 417:
#line 1071 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_PREOB_CAL,(yyvsp[0].prptr));}
#line 5076 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 418:
#line 1073 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_MIDOB_CAL,(yyvsp[0].miptr));}
#line 5082 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 419:
#line 1075 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_POSTOB_CAL,(yyvsp[0].poptr));}
#line 5088 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 420:
#line 1077 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                 {(yyval.lwptr)=make_lowl(T_PROCEDURE_NAME_PREFIX,(yyvsp[0].sval));}
#line 5094 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 421:
#line 1078 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 5100 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 422:
#line 1079 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 5106 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 423:
#line 1080 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 5112 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 424:
#line 1082 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5118 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 425:
#line 1084 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5124 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 426:
#line 1086 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5130 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 427:
#line 1088 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5136 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 428:
#line 1091 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.saptr)=make_setup_always((yyvsp[-3].sval),(yyvsp[-1].dvptr));}
#line 5142 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 429:
#line 1094 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.pcptr)=make_parity_check((yyvsp[-3].sval),(yyvsp[-1].dvptr));}
#line 5148 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 430:
#line 1097 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.tpptr)=make_tape_prepass((yyvsp[-3].sval),(yyvsp[-1].dvptr));}
#line 5154 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 431:
#line 1100 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.prptr)=make_preob_cal((yyvsp[-5].sval),(yyvsp[-3].dvptr),(yyvsp[-1].sval));}
#line 5160 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 432:
#line 1103 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.miptr)=make_midob_cal((yyvsp[-5].sval),(yyvsp[-3].dvptr),(yyvsp[-1].sval));}
#line 5166 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 433:
#line 1106 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.poptr)=make_postob_cal((yyvsp[-5].sval),(yyvsp[-3].dvptr),(yyvsp[-1].sval));}
#line 5172 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 434:
#line 1108 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 5178 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 435:
#line 1112 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 5184 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 436:
#line 1113 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=NULL;}
#line 5190 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 437:
#line 1115 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 5196 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 438:
#line 1116 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 5202 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 439:
#line 1118 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 5208 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 440:
#line 1119 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 5214 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 441:
#line 1120 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 5220 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 442:
#line 1123 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 5226 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 443:
#line 1125 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 5232 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 444:
#line 1127 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 5238 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 445:
#line 1128 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 5244 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 446:
#line 1130 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                   {(yyval.lwptr)=make_lowl(T_ROLL_REINIT_PERIOD,(yyvsp[0].dvptr));}
#line 5250 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 447:
#line 1131 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ROLL_INC_PERIOD,(yyvsp[0].dvptr));}
#line 5256 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 448:
#line 1132 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_ROLL,(yyvsp[0].sval));}
#line 5262 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 449:
#line 1133 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_ROLL_DEF,(yyvsp[0].llptr));}
#line 5268 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 450:
#line 1134 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 5274 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 451:
#line 1135 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 5280 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 452:
#line 1136 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 5286 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 453:
#line 1138 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5292 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 454:
#line 1140 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5298 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 455:
#line 1142 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 5304 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 456:
#line 1144 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=(yyvsp[-1].llptr);}
#line 5310 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 457:
#line 1149 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.llptr)=(yyvsp[0].llptr);}
#line 5316 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 458:
#line 1150 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.llptr)=NULL;}
#line 5322 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 459:
#line 1153 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 5328 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 460:
#line 1155 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 5334 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 461:
#line 1157 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 5340 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 462:
#line 1158 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 5346 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 463:
#line 1159 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 5352 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 464:
#line 1162 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 5358 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 465:
#line 1164 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 5364 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 466:
#line 1167 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 5370 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 467:
#line 1169 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 5376 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 468:
#line 1171 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 5382 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 469:
#line 1172 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_LITERAL,(yyvsp[0].llptr));}
#line 5388 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 470:
#line 1173 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 5394 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 471:
#line 1174 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 5400 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 472:
#line 1178 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 5406 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 473:
#line 1179 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 5412 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 474:
#line 1181 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 5418 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 475:
#line 1182 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 5424 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 476:
#line 1184 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 5430 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 477:
#line 1185 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 5436 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 478:
#line 1186 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 5442 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 479:
#line 1189 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 5448 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 480:
#line 1191 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 5454 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 481:
#line 1193 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 5460 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 482:
#line 1194 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 5466 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 483:
#line 1196 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_SEFD_MODEL,(yyvsp[0].sval));}
#line 5472 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 484:
#line 1197 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_SEFD,(yyvsp[0].septr));}
#line 5478 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 485:
#line 1198 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 5484 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 486:
#line 1199 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 5490 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 487:
#line 1200 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 5496 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 488:
#line 1202 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                            {(yyval.sval)=(yyvsp[-1].sval);}
#line 5502 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 489:
#line 1205 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.septr)=make_sefd((yyvsp[-5].sval),(yyvsp[-3].dvptr),(yyvsp[-1].llptr));}
#line 5508 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 490:
#line 1209 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[0].llptr);}
#line 5514 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 491:
#line 1210 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=NULL;}
#line 5520 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 492:
#line 1212 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 5526 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 493:
#line 1213 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 5532 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 494:
#line 1215 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 5538 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 495:
#line 1216 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 5544 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 496:
#line 1217 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 5550 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 497:
#line 1220 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 5556 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 498:
#line 1221 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 5562 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 499:
#line 1223 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 5568 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 500:
#line 1224 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 5574 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 501:
#line 1226 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_SITE_TYPE,(yyvsp[0].sval));}
#line 5580 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 502:
#line 1227 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_SITE_NAME,(yyvsp[0].sval));}
#line 5586 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 503:
#line 1228 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_SITE_ID,(yyvsp[0].sval));}
#line 5592 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 504:
#line 1229 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_SITE_POSITION,(yyvsp[0].spptr));}
#line 5598 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 505:
#line 1230 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                      {(yyval.lwptr)=make_lowl(T_SITE_POSITION_EPOCH,(yyvsp[0].sval));}
#line 5604 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 506:
#line 1231 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                      {(yyval.lwptr)=make_lowl(T_SITE_POSITION_REF,(yyvsp[0].sval));}
#line 5610 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 507:
#line 1232 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                      {(yyval.lwptr)=make_lowl(T_SITE_VELOCITY,(yyvsp[0].svptr));}
#line 5616 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 508:
#line 1233 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                 {(yyval.lwptr)=make_lowl(T_HORIZON_MAP_AZ,(yyvsp[0].llptr));}
#line 5622 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 509:
#line 1234 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                 {(yyval.lwptr)=make_lowl(T_HORIZON_MAP_EL,(yyvsp[0].llptr));}
#line 5628 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 510:
#line 1235 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_ZEN_ATMOS,(yyvsp[0].dvptr));}
#line 5634 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 511:
#line 1236 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_OCEAN_LOAD_VERT,(yyvsp[0].ovptr));}
#line 5640 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 512:
#line 1237 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_OCEAN_LOAD_HORIZ,(yyvsp[0].ohptr));}
#line 5646 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 513:
#line 1238 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_OCCUPATION_CODE,(yyvsp[0].sval));}
#line 5652 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 514:
#line 1239 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_INCLINATION,(yyvsp[0].dvptr));}
#line 5658 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 515:
#line 1240 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ECCENTRICITY,(yyvsp[0].dvptr));}
#line 5664 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 516:
#line 1241 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ARG_PERIGEE,(yyvsp[0].dvptr));}
#line 5670 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 517:
#line 1242 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ASCENDING_NODE,(yyvsp[0].dvptr));}
#line 5676 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 518:
#line 1243 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_MEAN_ANOMALY,(yyvsp[0].dvptr));}
#line 5682 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 519:
#line 1244 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_SEMI_MAJOR_AXIS,(yyvsp[0].dvptr));}
#line 5688 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 520:
#line 1245 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_MEAN_MOTION,(yyvsp[0].dvptr));}
#line 5694 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 521:
#line 1246 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ORBIT_EPOCH,(yyvsp[0].sval));}
#line 5700 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 522:
#line 1247 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 5706 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 523:
#line 1248 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 5712 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 524:
#line 1249 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 5718 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 525:
#line 1251 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                           {(yyval.sval)=(yyvsp[-1].sval);}
#line 5724 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 526:
#line 1253 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                           {(yyval.sval)=(yyvsp[-1].sval);}
#line 5730 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 527:
#line 1255 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                         {(yyval.sval)=(yyvsp[-1].sval);}
#line 5736 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 528:
#line 1259 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.spptr)=make_site_position((yyvsp[-5].dvptr),(yyvsp[-3].dvptr),(yyvsp[-1].dvptr));}
#line 5742 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 529:
#line 1261 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                             {(yyval.sval)=(yyvsp[-1].sval);}
#line 5748 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 530:
#line 1263 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                           {(yyval.sval)=(yyvsp[-1].sval);}
#line 5754 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 531:
#line 1267 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.svptr)=make_site_velocity((yyvsp[-5].dvptr),(yyvsp[-3].dvptr),(yyvsp[-1].dvptr));}
#line 5760 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 532:
#line 1269 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.llptr)=(yyvsp[-1].llptr);}
#line 5766 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 533:
#line 1271 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.llptr)=(yyvsp[-1].llptr);}
#line 5772 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 534:
#line 1273 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5778 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 535:
#line 1277 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.ovptr)=make_ocean_load_vert((yyvsp[-3].dvptr),(yyvsp[-1].dvptr));}
#line 5784 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 536:
#line 1281 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.ohptr)=make_ocean_load_horiz((yyvsp[-3].dvptr),(yyvsp[-1].dvptr));}
#line 5790 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 537:
#line 1283 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                             {(yyval.sval)=(yyvsp[-1].sval);}
#line 5796 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 538:
#line 1285 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5802 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 539:
#line 1287 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5808 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 540:
#line 1289 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5814 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 541:
#line 1291 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5820 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 542:
#line 1293 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5826 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 543:
#line 1295 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                             {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5832 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 544:
#line 1297 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 5838 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 545:
#line 1299 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                             {(yyval.sval)=(yyvsp[-1].sval);}
#line 5844 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 546:
#line 1303 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=(yyvsp[0].llptr);}
#line 5850 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 547:
#line 1304 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=NULL;}
#line 5856 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 548:
#line 1306 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 5862 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 549:
#line 1307 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 5868 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 550:
#line 1309 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 5874 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 551:
#line 1310 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 5880 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 552:
#line 1311 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 5886 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 553:
#line 1314 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 5892 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 554:
#line 1316 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 5898 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 555:
#line 1318 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 5904 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 556:
#line 1319 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 5910 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 557:
#line 1321 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_SOURCE_TYPE,(yyvsp[0].llptr));}
#line 5916 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 558:
#line 1322 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_SOURCE_NAME,(yyvsp[0].sval));}
#line 5922 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 559:
#line 1323 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_IAU_NAME,(yyvsp[0].sval));}
#line 5928 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 560:
#line 1324 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_RA,(yyvsp[0].sval));}
#line 5934 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 561:
#line 1325 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEC,(yyvsp[0].sval));}
#line 5940 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 562:
#line 1326 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF_COORD_FRAME,(yyvsp[0].sval));}
#line 5946 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 563:
#line 1327 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                      {(yyval.lwptr)=make_lowl(T_SOURCE_POSITION_REF,(yyvsp[0].sval));}
#line 5952 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 564:
#line 1328 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_SOURCE_POSITION_EPOCH,(yyvsp[0].sval));}
#line 5958 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 565:
#line 1329 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_RA_RATE,(yyvsp[0].dvptr));}
#line 5964 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 566:
#line 1330 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEC_RATE,(yyvsp[0].dvptr));}
#line 5970 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 567:
#line 1331 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_VELOCITY_WRT_LSR,(yyvsp[0].dvptr));}
#line 5976 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 568:
#line 1332 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_SOURCE_MODEL,(yyvsp[0].smptr));}
#line 5982 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 569:
#line 1333 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_INCLINATION,(yyvsp[0].dvptr));}
#line 5988 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 570:
#line 1334 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ECCENTRICITY,(yyvsp[0].dvptr));}
#line 5994 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 571:
#line 1335 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ARG_PERIGEE,(yyvsp[0].dvptr));}
#line 6000 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 572:
#line 1336 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ASCENDING_NODE,(yyvsp[0].dvptr));}
#line 6006 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 573:
#line 1337 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_MEAN_ANOMALY,(yyvsp[0].dvptr));}
#line 6012 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 574:
#line 1338 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_SEMI_MAJOR_AXIS,(yyvsp[0].dvptr));}
#line 6018 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 575:
#line 1339 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_MEAN_MOTION,(yyvsp[0].dvptr));}
#line 6024 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 576:
#line 1340 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_ORBIT_EPOCH,(yyvsp[0].sval));}
#line 6030 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 577:
#line 1341 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 6036 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 578:
#line 1342 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 6042 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 579:
#line 1343 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 6048 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 580:
#line 1345 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[-1].sval));}
#line 6054 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 581:
#line 1347 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(add_list(NULL,(yyvsp[-3].sval)),(yyvsp[-1].sval));}
#line 6060 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 582:
#line 1349 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 6066 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 583:
#line 1351 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 6072 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 584:
#line 1353 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 6078 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 585:
#line 1355 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 6084 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 586:
#line 1357 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 6090 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 587:
#line 1359 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 6096 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 588:
#line 1361 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.sval)=(yyvsp[-1].sval);}
#line 6102 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 589:
#line 1363 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 6108 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 590:
#line 1365 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 6114 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 591:
#line 1368 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.dvptr)=(yyvsp[-1].dvptr);}
#line 6120 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 592:
#line 1378 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.smptr)=make_source_model((yyvsp[-15].dvptr),(yyvsp[-13].sval),(yyvsp[-11].dvptr),(yyvsp[-9].dvptr),(yyvsp[-7].dvptr),(yyvsp[-5].dvptr),(yyvsp[-3].dvptr),(yyvsp[-1].dvptr));}
#line 6126 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 593:
#line 1382 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.llptr)=(yyvsp[0].llptr);}
#line 6132 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 594:
#line 1383 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                                {(yyval.llptr)=NULL;}
#line 6138 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 595:
#line 1386 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 6144 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 596:
#line 1387 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 6150 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 597:
#line 1389 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 6156 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 598:
#line 1390 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 6162 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 599:
#line 1391 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 6168 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 600:
#line 1395 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 6174 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 601:
#line 1397 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 6180 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 602:
#line 1400 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 6186 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 603:
#line 1401 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 6192 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 604:
#line 1403 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_VSN,(yyvsp[0].vsptr));}
#line 6198 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 605:
#line 1404 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 6204 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 606:
#line 1405 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 6210 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 607:
#line 1407 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                 {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 6216 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 608:
#line 1410 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.vsptr)=make_vsn((yyvsp[-7].dvptr),(yyvsp[-5].sval),(yyvsp[-3].sval),(yyvsp[-1].sval));}
#line 6222 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 609:
#line 1414 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=(yyvsp[0].llptr);}
#line 6228 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 610:
#line 1415 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=NULL;}
#line 6234 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 611:
#line 1417 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 6240 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 612:
#line 1418 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 6246 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 613:
#line 1420 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DEF,(yyvsp[0].dfptr));}
#line 6252 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 614:
#line 1421 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 6258 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 615:
#line 1422 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 6264 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 616:
#line 1425 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                        {(yyval.dfptr)=make_def((yyvsp[-4].sval),(yyvsp[-2].llptr));}
#line 6270 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 617:
#line 1427 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                       {(yyval.dfptr)=make_def((yyvsp[-3].sval),NULL);}
#line 6276 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 618:
#line 1429 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-1].llptr),(yyvsp[0].lwptr));}
#line 6282 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 619:
#line 1430 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].lwptr));}
#line 6288 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 620:
#line 1432 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_FANIN_DEF,(yyvsp[0].fiptr));}
#line 6294 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 621:
#line 1433 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_FANOUT_DEF,(yyvsp[0].foptr));}
#line 6300 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 622:
#line 1435 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_TRACK_FRAME_FORMAT,(yyvsp[0].sval));}
#line 6306 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 623:
#line 1436 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_DATA_MODULATION,(yyvsp[0].sval));}
#line 6312 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 624:
#line 1438 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_VLBA_FRMTR_SYS_TRK,(yyvsp[0].fsptr));}
#line 6318 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 625:
#line 1440 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_VLBA_TRNSPRT_SYS_TRK,(yyvsp[0].llptr));}
#line 6324 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 626:
#line 1441 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_S2_RECORDING_MODE,(yyvsp[0].sval));}
#line 6330 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 627:
#line 1442 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.lwptr)=make_lowl(T_S2_DATA_SOURCE,(yyvsp[0].dsptr));}
#line 6336 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 628:
#line 1443 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_REF,(yyvsp[0].exptr));}
#line 6342 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 629:
#line 1444 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                {(yyval.lwptr)=make_lowl(T_COMMENT,(yyvsp[0].sval));}
#line 6348 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 630:
#line 1445 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                     {(yyval.lwptr)=make_lowl(T_COMMENT_TRAILING,(yyvsp[0].sval));}
#line 6354 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 631:
#line 1448 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.fiptr)=make_fanin_def((yyvsp[-7].sval),(yyvsp[-5].dvptr),(yyvsp[-3].dvptr),(yyvsp[-1].llptr));}
#line 6360 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 632:
#line 1452 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.foptr)=make_fanout_def((yyvsp[-7].sval),(yyvsp[-5].llptr),(yyvsp[-3].dvptr),(yyvsp[-1].llptr));}
#line 6366 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 633:
#line 1455 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                  {(yyval.foptr)=make_fanout_def(NULL,(yyvsp[-5].llptr),(yyvsp[-3].dvptr),(yyvsp[-1].llptr));}
#line 6372 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 634:
#line 1457 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                            {(yyval.sval)=(yyvsp[-1].sval);}
#line 6378 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 635:
#line 1459 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                         {(yyval.sval)=(yyvsp[-1].sval);}
#line 6384 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 636:
#line 1463 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                        {(yyval.fsptr)=make_vlba_frmtr_sys_trk((yyvsp[-7].dvptr),(yyvsp[-5].sval),(yyvsp[-3].dvptr),(yyvsp[-1].dvptr));}
#line 6390 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 637:
#line 1466 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                        {(yyval.fsptr)=make_vlba_frmtr_sys_trk((yyvsp[-5].dvptr),(yyvsp[-3].sval),(yyvsp[-1].dvptr),NULL);}
#line 6396 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 638:
#line 1469 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                        {(yyval.llptr)=add_list(add_list(NULL,(yyvsp[-3].dvptr)),(yyvsp[-1].dvptr));}
#line 6402 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 639:
#line 1471 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                           {(yyval.sval)=(yyvsp[-1].sval);}
#line 6408 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 640:
#line 1474 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.dsptr)=make_s2_data_source((yyvsp[-5].sval),(yyvsp[-3].sval),(yyvsp[-1].sval));}
#line 6414 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 641:
#line 1476 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                {(yyval.dsptr)=make_s2_data_source((yyvsp[-1].sval),NULL,NULL);}
#line 6420 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 642:
#line 1479 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(add_list((yyvsp[-4].llptr),(yyvsp[-2].sval)),(yyvsp[0].sval));}
#line 6426 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 643:
#line 1481 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=add_list(add_list(NULL,(yyvsp[-2].sval)),(yyvsp[0].sval));}
#line 6432 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 644:
#line 1486 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.exptr)=make_external((yyvsp[-5].sval),(yyvsp[-3].ival),(yyvsp[-1].sval));}
#line 6438 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 645:
#line 1488 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.llptr)=(yyvsp[-1].llptr);}
#line 6444 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 646:
#line 1490 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=ins_list((yyvsp[-2].dvptr),(yyvsp[0].llptr));}
#line 6450 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 647:
#line 1491 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].dvptr));}
#line 6456 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 648:
#line 1493 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-2].llptr),(yyvsp[0].dvptr));}
#line 6462 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 649:
#line 1494 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].dvptr));}
#line 6468 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 650:
#line 1496 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=(yyvsp[0].dvptr);}
#line 6474 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 652:
#line 1499 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                        {(yyval.dvptr)=make_dvalue((yyvsp[-1].sval),(yyvsp[0].sval));}
#line 6480 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 653:
#line 1501 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-2].llptr),(yyvsp[0].sval));}
#line 6486 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 654:
#line 1502 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].sval));}
#line 6492 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 656:
#line 1506 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list((yyvsp[-2].llptr),(yyvsp[0].dvptr));}
#line 6498 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 657:
#line 1507 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.llptr)=add_list(NULL,(yyvsp[0].dvptr));}
#line 6504 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;

  case 658:
#line 1509 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"
                                                {(yyval.dvptr)=make_dvalue((yyvsp[0].sval),NULL);}
#line 6510 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"
    break;


#line 6514 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 1511 "/home/ntsikelelo/Projects/ALBUS/ALBUS_ionosphere/C++/vex_to_sky2/vex2005/vex.y"


yyerror(s)
char *s;
{
  fprintf(stderr,"%s at line %d\n",s,lines);
  exit(1);
}


