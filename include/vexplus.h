//-------------------------------------------------------------------
//
// $Id: vexplus.h,v 3.4 2005/06/07 12:20:30 olnon Exp $
// 
// Original File: vexcjd.h 
// Original Date: 8-1-1998 Author: Henk W. Klijn Hesselink
// Taken over: 03MAR1998 Huib Jan van Langevelde
// Rewrite: 23 Mar 1998 Huib Jan van Langevelde
// 980429fmo: Taken over by Friso Olnon
// 2005 Sep 20  James M Anderson  --use M_PI for PI
//
//-------------------------------------------------------------------

#ifndef VEXPLUS_H
#define VEXPLUS_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <iostream>
#include "vex++.h"

#include <string>

//const double  PI  = 4.0*atan(1.0);
//const double  SPEED_OF_LIGHT = 299792458.0;
//const int     SECS_PER_DAY = (3600*24);
//const int     IPS_FEET = 12;
//
// Doesn't seem to work with gnu (fmo011016); so, back to the old defines.
//
//#define PI (3.14159265358979323846) //copied from prep_job/calc_model.h
#define PI M_PI
#define SPEED_OF_LIGHT (299792458.0)
#define SECS_PER_DAY (86400)
#define IPS_FEET (12)

//-------------------------------------------------------------------
// Class        : VexPlus
// Description  : Wrapper around vex reading stuff
//-------------------------------------------------------------------
class VexPlus
{

public:

    VexPlus( const std::string& vexfilename)
            : filename(vexfilename), vexp(0)
    {
    }

    //________________GENERAL_ACCESS_________________________________

    // Open Vex file and parse it to memory
    int    parseVex();


    /////////////////////////////////////////////////////////////////
    // FIRST VEX LINE
    //
    // identifies the file type and must be of the form:
    //
    // VEX_rev = <VEX rev level>;
    //
    // followed by an optional comment (prefaced with a `*').

    // Version number
    // 980814fmo: not implemented and not used!?!
    char   *get_version_number();
    
    /////////////////////////////////////////////////////////////////
    // HIGH-LEVEL VEX BLOCKS________________________________________
    //
    // are made up entirely of references to primitive-block keywords.
    // The combination of a $STATION key, $MODE key, plus the $GLOBAL
    // parameters specify the detailed configuration for an
    // observation at a particular station.

    //________________$GLOBAL________________________________________
    //
    // Global/general experiment parameters.

    //________________$STATION_______________________________________
    //
    // Station keywords.

    // Total number of stations in VEX
    int    N_Stations( ) const;

    // Name of the icount+1 station in the list
   std::string Station( const int& icount ) const;

    //________________$MODE__________________________________________
    //
    // Mode keywords.

    // Number of modes in VEX file
    int    N_Modes( ) const;

    // icount+1 mode in the list
   std::string Mode( const int& icount ) const;

    /////////////////////////////////////////////////////////////////
    // SCHEDULE VEX BLOCK____________________________________________
    //
    // Detailed time-ordered list of observations, using keyword
    // references to $STATION, $MODE and $SOURCE `defs'.

    //________________$SCHED_________________________________________
    //
    // Observation schedule.

    // Number of scans in VEX file
    int    N_Scans( const std::string& stat ) const;

    // Name for icount+1st scan
   std::string ScanName(const std::string& stat, const int& icount ) const;
   std::string ScanName(const int& icount ) const;

    // Start time for icount+1st scan
   std::string ScanStart(const std::string& stat, const int& icount ) const;

    // Mode for icount+1st scan
   std::string ScanMode(const std::string& stat, const int& icount ) const;

    // Source for icount+1st scan
   std::string ScanSource(const std::string& stat, const int& icount ) const;

    // Pass id for icount+1st scan and station stat
   std::string  ScanPass(const std::string& stat, const int& icount ) const;

    // Duration for icount+1st scan and station stat
    double ScanDuration(const std::string& stat, const int& icount ) const;

    // Footage icount+1st scan and station stat
    double Footage(const std::string& stat, const int& icount ) const;

    // Data start offset icount+1st scan and station stat
    double DataStart(const std::string& stat, const int& icount ) const;

    /////////////////////////////////////////////////////////////////
    // PRIMITIVE VEX BLOCKS__________________________________________
    //
    // serve to define low-level station, source , and recording
    // parameters. They consist entirely of keyword `def blocks' with
    // no direct reference to any other $blocks (except perhaps to an
    // external database file).

    //________________$ANTENNA_______________________________________
    //
    // Antenna parameters. Required in scheduling and correlation.

    // Mount of the antenna identified with stat
   std::string AxisMount( const std::string& stat ) const;

    // Axis offset of the antenna
    double AxisOffset( const std::string& stat ) const;

    //________________$BBC___________________________________________
    //
    // BBC/IF assignments. Required in scheduling and data-taking.

    // Number of bbcs in a BBC ref
    int    N_BBCs( const std::string& stat, const std::string& mode ) const;

    // Linkname that links a chan_def to an if
   std::string Link_bbc_if( const std::string& stat, const std::string& mode,
                        const int& icount ) const;

    // Linkname set for a bbc link from frequency
   std::string Resolve_bbc_freq( const std::string& stat, const std::string& mode,
			      const int& icount ) const;

    //________________$CLOCK_________________________________________
    //
    // Clock-synchronization model for correlation. Required in
    // correlation.
    //
    // For the moment we assume: no 'clock breaks'; so the model is
    // valid for the entire experiment.

    // Offset in sec of stat's formatter clock
    double ClockOffset( const std::string& stat ) const;

    // Offset rate in microsec/sec of stat's formatter clock
    double ClockRate( const std::string& stat ) const;

    // Epoch in VEX-character string format of origin of clock model
   std::string ClockEpoch( const std::string& stat ) const;

    //________________$CORR__________________________________________
    //
    // Correlation parameters. Required in correlation.

    //________________$DAS___________________________________________
    //
    // Data-acquisition system information. Required in scheduling,
    // data-taking and correlation.

    // Recorder type used at stat
   std::string Recorder( const std::string& stat ) const;

    // Length of tape used at stat
    double MediumLength( const std::string& stat ) const;

    // Density of tape recorded at stat
    double TapeDensity( const std::string& stat ) const;

    //________________$EOP___________________________________________
    //
    // Earth-orientation information. Required in correlation.

    // TAI-UTC in seconds
    double TAI_UTC() const;

    // A1_TAI in seconds
    double A1_TAI() const;

    // EOP epoch
   std::string EOPEpoch() const;

    // Number of EOP points
    int    N_EOP_Points() const;

    // Interval of the EOP points in hours
    double EOP_interval() const;

    // icount-1 UT1-UTC value in sec
    double UT1_UTC( const int& icount ) const;

    // icount-1 xwobble value in asec
    double XWobble( const int& icount ) const;

    // icount-1 ywobble value in asec
    double YWobble( const int& icount ) const;

    //________________$EXPER_________________________________________
    //
    // General experiment information. Required in scheduling,
    // data-taking and correlation.

    // Experiment number
    /// int num = aCJD.get_experiment_ID().get_exper_num();
    int    ExperNo() const;

    // Experiment name
    /// char name[7];
    /// aCJD.get_experiment_ID().get_exper_name(name);
   std::string ExperName() const;
   std::string PIName() const;

    //________________$FREQ__________________________________________
    //
    // Channel frequencies, bandwidth, sample rates, etc. Required in
    // scheduling, data-taking and correlation.

    // Number of channels in a freq ref
    int    N_FreqChans( const std::string& stat, const std::string& mode ) const;
    
    // Total LO value (band edge) in MHz
    double SkyFreq( const std::string& stat, const std::string& mode,
                    const int& icount ) const;
    
    // Sample rate in Ms/s
    double SampleRate( const std::string& stat, const std::string& mode ) const;
    
    // Filter width in Hz
    double BW( const std::string& stat, const std::string& mode,
               const int& icount ) const;
    
    // Net sideband as a string
   std::string SideBand( const std::string& stat, const std::string& mode,
                     const int& icount ) const;
    
    // Linkname that links a chan_def to a track
   std::string Link_freq_track( const std::string& stat, const std::string& mode,
                            const int& icount ) const;
    
    // Linkname that links a chan_def to a bbc
   std::string Link_freq_bbc( const std::string& stat, const std::string& mode,
                          const int& icount ) const;
    
    //________________$HEAD_POS______________________________________
    //
    // Headstack-positioning information. Required in scheduling,
    // data-taking and correlation.

    // Number of Headstack positions
    int    N_HeadPositions( const std::string& stat, const std::string& mode ) const;
    
    // Headstack position index number icount-1 for station and mode
    int    HeadIndex( const std::string& stat, const std::string& mode, 
                      const int& icount ) const;
    
    // Headstack position icount-1 for station and mode
    double HeadPosition( const std::string& stat, const std::string& mode, 
                         const int& icount ) const;

    // Headstack position icount-1 for station, mode and headstack number (1-4)
    double HeadPosition( const std::string& stat, const std::string& mode, int hs,
                         const int& icount ) const;

    //________________$IF____________________________________________
    //
    // IF bands/sidebands, first LO freq, phase-cal freqs. Required in
    // scheduling, data-taking and correlation.

    // Number of channels in a if ref
    int    N_IFs( const std::string& stat, const std::string& mode ) const;

    // Linkname that links an if to a bbc
    std::string Resolve_if_bbc( const std::string& stat, const std::string& mode,
			   const int& icount ) const; 

    // Polarization in IF block
    std::string Pol(  const std::string& stat, const std::string& mode,
                 const int& icount ) const;

    // Polarization, given the iband in a FREQ def
    std::string Pol_by_Freq( const std::string& stat, const std::string& mode, 
			const int& icount ) const;

    // Phase-cal frequency interval in IF block
    double PCalFreqInterval (const std::string& stat,
			     const std::string& mode,
			     const int& icount) const;
  
    // Phase-cal frequency interval, given the iband in a FREQ def
    double PCalFreqInterval_by_Freq (const std::string& stat,
				     const std::string& mode,
				     const int& icount) const;

    // Phase-cal base frequency in IF block
    double PCalFreqBase (const std::string& stat,
			 const std::string& mode,
			 const int& icount) const;
  
    // Phase-cal base frequency, given the iband in a FREQ def
    double PCalFreqBase_by_Freq (const std::string& stat,
				 const std::string& mode,
				 const int& icount) const;
  

    //________________$PHASE_CAL_DETECT______________________________
    //
    // Phase-cal frequencies to be detected. Required in scheduling,
    // data-taking and correlation.

    //________________$PASS_ORDER____________________________________
    //
    // Tape pass order. Required in scheduling, data-taking and
    // correlation.

    // Number of passes
    int    N_Passes( const std::string& stat, const std::string& mode ) const;

    // Pass name icount-1 for station and mode
    std::string  Pass( const std::string& stat, const std::string& mode, 
		  const int& icount ) const;
 
    //________________$PROCEDURES____________________________________
    //
    // General-procedure timing parameters. Required in scheduling and
    // data-taking.

    //________________$ROLL__________________________________________
    //
    // Recording barrel-roll details. Required in scheduling,
    // data-taking and correlation.

    // Roll on/off
    int    Roll( const std::string& stat, const std::string& mode ) const;

    //________________$SCHEDULING_PARAMS_____________________________
    //
    // Parameters for scheduling program. Required in scheduling.

    //________________$SEFD__________________________________________
    //
    // SEFD info. Required in scheduling.

    //________________$SITE__________________________________________
    //
    // Antenna site details. Required in scheduling and correlation.

    // Site name for a station
    std::string  Site( const std::string& stat ) const;

    // Two letter code = site ID for a station
    std::string  TwoLetter( const std::string& stat ) const;

    // Site X position for a station
    double SiteX( const std::string& stat ) const;

    // Site Y position for a station
    double SiteY( const std::string& stat ) const;

    // Site Z position for a station
    double SiteZ( const std::string& stat ) const;

    //________________$SOURCE________________________________________
    //
    // Source names, positions, etc. Required in scheduling,
    // data-taking and correlation.

    // Total number of sources in VEX
    int    N_Sources( ) const;

    // Name of the icount+1 source in the list
    std::string SourceName( const int& icount ) const;

    // Reference epoch of the icount+1 source in the list
    std::string RefEpoch( const int& icount ) const;

    // Type of the icount+1 source in the list
    std::string SourceType( const int& icount ) const;

    // IAU name of the icount+1 source in the list
    std::string IAUName( const int& icount ) const;

    // Right Ascension of the icount+1 source in radians
    double Source_RA( const int& icount ) const;
  
    // Declination of the icount+1 source in radians
    double Source_Dec( const int& icount ) const;

    // Right Ascencion rate of the icount+1 source in radians/yr
    double Source_RARate( const int& icount ) const;

    // Declination rate of the icount+1 source in radians/yr
    double Source_DecRate( const int& icount ) const;

    // Source epoch of the icount+1 source
    std::string Source_Epoch( const int& icount ) const;

    // LSR velocity of the icount+1 source
    double Vel_LSR( const int& icount ) const;

    //________________$TAPELOG_OBS___________________________________
    //
    // `As-observed' tape-usage log from the `.obs' files.

    // Number of VSNs in a BBC ref
    int    N_VSNs( const std::string& stat ) const;

    // Tape volume serial number as a string.
    std::string  VSN( const std::string& stat, const int& icount ) const;

    // Tape start time
    std::string VSN_start( const std::string& stat, const int& icount ) const;

    // Tape stop time
    std::string VSN_end( const std::string& stat, const int& icount ) const;

    //________________$TRACKS________________________________________
    //
    // Recording multiplex details. Required in scheduling,
    // data-taking and correlation.

    // Number of fan_out def lines in Track def
    int    N_TrackLines( const std::string& stat, const std::string& mode ) const;

    // Number of Bitstream from Track def
    int    N_BitStreams( const std::string& stat, const std::string& mode ) const;

    // Number of Bitstream from Track def
    int    N_Bits( const std::string& stat, const std::string& mode ) const;

    // Fan out factor
    int    FanOut( const std::string& stat, const std::string& mode ) const;

    // Total number of tracks
    int    N_Tracks( const std::string& stat, const std::string& mode ) const;

    // 'Chan-ID' linkword for the icount-th fanout_def line
    std::string  Resolve_track_freq( const std::string& stat, const std::string& mode,
				const int& icount ) const;

    // 'Chan-ID' linkword for track icount
    std::string Track( const std::string& stat, const std::string& mode,
                  const int& icount ) const;

    // Subpass code for the icount-th fanout_def line
    std::string TrackSubpass( const std::string& stat, const std::string& mode,
			 const int& icount ) const;

    // Bitstream type for the icount-th fanout_def line
    std::string TrackSignMag( const std::string& stat, const std::string& mode,
			 const int& icount ) const;

    // Headstack number for the icount-th fanout_def line
    int HeadstackNr( const std::string& stat, const std::string& mode,
                     const int& icount) const;

    // Frame format on tape track
    std::string  TrackFormat( const std::string& stat, const std::string& mode ) const;

    // Modulation on/off
    int    Modulation( const std::string& stat, const std::string& mode ) const;

    // Number of the icount's (0-based) track
    int    TrackNr( const std::string& stat, const std::string& mode,
                    const int& icount) const;


    
protected:
    
    VexPlus( ) {};  
    std::string filename;
    struct vex *vexp;
    
};

// Convert a frequency string-value into a double (in MHz)
double freqinMHz(const std::string& freqin, const std::string& unitsin);

// Convert a frequency string-value into a double (in Hz)
double freqinHz(const std::string& freqin, const std::string& unitsin);

#endif




