#lang racket

(require ffi/unsafe
         racket/runtime-path
         (for-syntax syntax/parse)
         (only-in '#%foreign ffi-callback))

(provide (all-defined-out))

(define-runtime-path lib-path "lib")
;; use local copies of the libraries for Windows & Mac...
(define win-dll-path (build-path lib-path (system-library-subpath) "portaudio"))
(define mac-dll-path (build-path lib-path "libportaudio"))

(define linux-err-msg
  "Note: on Linux, you need to install the libportaudio library yourself. Underlying error message: ~a")

(define libportaudio
  (case (system-type)
    [(windows) (ffi-lib win-dll-path)]
    [(macosx)  (with-handlers ()
                 (ffi-lib mac-dll-path '("2" "")))]
    [(unix)    (with-handlers 
                   ([exn:fail? 
                     (lambda (exn)
                       (error 'rsound 
                               linux-err-msg
                              (exn-message exn)))])
                 (ffi-lib "libportaudio" '("2.0.0" "")))]))

;; wrap a function to signal an error when an error code is returned.
;; (any ... -> pa-error) -> (any ... -> )
(define (pa-checked pa-fun name)
  (lambda args
    (match (apply pa-fun args)
      ['paNoError (void)]
      [(? symbol? s) (error (pa-get-error-text s))]
      [other (error name
                    "internal error: expected a symbol, got: ~s"
                    other)])))

;; a convenience abstraction for defining checked functions.
(define-syntax (define-checked stx)
  (syntax-parse stx
    [(_ name:id binding:expr)
     (with-syntax ([name-as-symbol #`(#%datum . #,(syntax-e #'name))])
       #`(define name
           (pa-checked binding name-as-symbol)))]))



;; wrap a function to signal an error when an integer < 0 is returned.
;; only apply it to things that return ints
(define (pa-semi-checked pa-fun name)
  (lambda args
    (define result (apply pa-fun args))
    (cond [(not (integer? result))
           (error 'name 
                  "internal error: checker applied to fun returning non-int: ~e"
                  result)]
          [(< result 0)
           (error (pa-get-error-text/int result))]
          [else
           result])))

;; a convenience abstraction for defining semi-checked functions.
(define-syntax (define-semi-checked stx)
  (syntax-parse stx
    [(_ name:id binding:expr)
     (with-syntax ([name-as-symbol #`(#%datum . #,(syntax-e #'name))])
       #`(define name
           (pa-semi-checked binding name-as-symbol)))]))



;; headers taken from 19.20110326 release of portaudio.h

;; note that every line of the header file appears here verbatim; this means
;; that you can 'diff' this file against the header file, and check that the
;; only differences are additions to verify that the header file is up-to-date.
;; (Yes, this can fail, but only if the racket part of the file contains the
;; newly appeared header file line, which seems exceedingly unlikely.)

;; initial block:
#|
#ifndef PORTAUDIO_H
#define PORTAUDIO_H
/*
 * $Id: portaudio.h 1594 2011-02-05 14:33:29Z rossb $
 * PortAudio Portable Real-Time Audio Library
 * PortAudio API Header File
 * Latest version available at: http://www.portaudio.com/
 *
 * Copyright (c) 1999-2002 Ross Bencina and Phil Burk
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * The text above constitutes the entire PortAudio license; however, 
 * the PortAudio community also makes the following non-binding requests:
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version. It is also 
 * requested that these non-binding requests be included along with the 
 * license above.
 */

/** @file
 @ingroup public_header
 @brief The portable PortAudio API.
*/


#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

 
/** Retrieve the release number of the currently running PortAudio build,
 eg 1900.
*/
int Pa_GetVersion( void );
|#
(define pa-get-version 
  (get-ffi-obj "Pa_GetVersion" 
               libportaudio
               (_fun -> _int)))

#|

/** Retrieve a textual description of the current PortAudio build,
 eg "PortAudio V19-devel 13 October 2002".
*/
const char* Pa_GetVersionText( void );
|#
(define pa-get-version-text
    (get-ffi-obj "Pa_GetVersionText"
                 libportaudio
                 (_fun -> _string)))


#|
/** Error codes returned by PortAudio functions.
 Note that with the exception of paNoError, all PaErrorCodes are negative.
*/

typedef int PaError;
typedef enum PaErrorCode
{
    paNoError = 0,

    paNotInitialized = -10000,
    paUnanticipatedHostError,
    paInvalidChannelCount,
    paInvalidSampleRate,
    paInvalidDevice,
    paInvalidFlag,
    paSampleFormatNotSupported,
    paBadIODeviceCombination,
    paInsufficientMemory,
    paBufferTooBig,
    paBufferTooSmall,
    paNullCallback,
    paBadStreamPtr,
    paTimedOut,
    paInternalError,
    paDeviceUnavailable,
    paIncompatibleHostApiSpecificStreamInfo,
    paStreamIsStopped,
    paStreamIsNotStopped,
    paInputOverflowed,
    paOutputUnderflowed,
    paHostApiNotFound,
    paInvalidHostApi,
    paCanNotReadFromACallbackStream,
    paCanNotWriteToACallbackStream,
    paCanNotReadFromAnOutputOnlyStream,
    paCanNotWriteToAnInputOnlyStream,
    paIncompatibleStreamHostApi,
    paBadBufferPtr
} PaErrorCode;
|#

(define pa-not-initialized-error -10000)
(define _pa-error
  (_enum
   `(paNoError = 0
               
     paNotInitialized = ,pa-not-initialized-error
     paUnanticipatedHostError
     paInvalidChannelCount
     paInvalidSampleRate
     paInvalidDevice
     paInvalidFlag
     paSampleFormatNotSupported
     paBadIODeviceCombination
     paInsufficientMemory
     paBufferTooBig
     paBufferTooSmall
     paNullCallback
     paBadStreamPtr
     paTimedOut
     paInternalError
     paDeviceUnavailable
     paIncompatibleHostApiSpecificStreamInfo
     paStreamIsStopped
     paStreamIsNotStopped
     paInputOverflowed
     paOutputUnderflowed
     paHostApiNotFound
     paInvalidHostApi
     paCanNotReadFromACallbackStream     ;; /**< @todo review error code name */
     paCanNotWriteToACallbackStream      ;; /**< @todo review error code name */
     paCanNotReadFromAnOutputOnlyStream  ;; /**< @todo review error code name */
     paCanNotWriteToAnInputOnlyStream    ;; /**< @todo review error code name */
     paIncompatibleStreamHostApi
     paBadBufferPtr)
   _int))

#|
/** Translate the supplied PortAudio error code into a human readable
 message.
*/
const char *Pa_GetErrorText( PaError errorCode );
|#
(define pa-get-error-text
  (get-ffi-obj "Pa_GetErrorText"
               libportaudio
               (_fun _pa-error -> _string)))

;; useful when you have the result as an int rather
;; than a symbol, as e.g. when returned by 
;; pa-get-host-api-count
(define pa-get-error-text/int
  (get-ffi-obj "Pa_GetErrorText"
               libportaudio
               (_fun _int -> _string)))

#|

/** Library initialization function - call this before using PortAudio.
 This function initializes internal data structures and prepares underlying
 host APIs for use.  With the exception of Pa_GetVersion(), Pa_GetVersionText(),
 and Pa_GetErrorText(), this function MUST be called before using any other
 PortAudio API functions.

 If Pa_Initialize() is called multiple times, each successful 
 call must be matched with a corresponding call to Pa_Terminate(). 
 Pairs of calls to Pa_Initialize()/Pa_Terminate() may overlap, and are not 
 required to be fully nested.

 Note that if Pa_Initialize() returns an error code, Pa_Terminate() should
 NOT be called.

 @return paNoError if successful, otherwise an error code indicating the cause
 of failure.

 @see Pa_Terminate
*/
PaError Pa_Initialize( void );
|#

(define-checked pa-initialize 
  (get-ffi-obj "Pa_Initialize" 
               libportaudio
               (_fun -> _pa-error)))

#|
/** Library termination function - call this when finished using PortAudio.
 This function deallocates all resources allocated by PortAudio since it was
 initialized by a call to Pa_Initialize(). In cases where Pa_Initialise() has
 been called multiple times, each call must be matched with a corresponding call
 to Pa_Terminate(). The final matching call to Pa_Terminate() will automatically
 close any PortAudio streams that are still open.

 Pa_Terminate() MUST be called before exiting a program which uses PortAudio.
 Failure to do so may result in serious resource leaks, such as audio devices
 not being available until the next reboot.

 @return paNoError if successful, otherwise an error code indicating the cause
 of failure.
 
 @see Pa_Initialize
*/
PaError Pa_Terminate( void );
|#

(define-checked pa-terminate
  (get-ffi-obj "Pa_Terminate"
               libportaudio
               (_fun -> _pa-error)))



#|
/** The type used to refer to audio devices. Values of this type usually
 range from 0 to (Pa_GetDeviceCount()-1), and may also take on the PaNoDevice
 and paUseHostApiSpecificDeviceSpecification values.

 @see Pa_GetDeviceCount, paNoDevice, paUseHostApiSpecificDeviceSpecification
*/
typedef int PaDeviceIndex;


/** A special PaDeviceIndex value indicating that no device is available,
 or should be used.

 @see PaDeviceIndex
*/
#define paNoDevice ((PaDeviceIndex)-1)


/** A special PaDeviceIndex value indicating that the device(s) to be used
 are specified in the host api specific stream info structure.

 @see PaDeviceIndex
*/
#define paUseHostApiSpecificDeviceSpecification ((PaDeviceIndex)-2)
|#

(define _pa-device-index _int)
(define _pa-no-device -1)
(define _pa-use-host-api-specific-device-specification -2)
;; these literal numbers also appear below in 'get-default-<i/o>-device'

#|

/* Host API enumeration mechanism */

/** The type used to enumerate to host APIs at runtime. Values of this type
 range from 0 to (Pa_GetHostApiCount()-1).

 @see Pa_GetHostApiCount
*/
typedef int PaHostApiIndex;
|#
(define _pa-host-api-index _int)
#|


/** Retrieve the number of available host APIs. Even if a host API is
 available it may have no devices available.

 @return A non-negative value indicating the number of available host APIs
 or, a PaErrorCode (which are always negative) if PortAudio is not initialized
 or an error is encountered.

 @see PaHostApiIndex
*/
PaHostApiIndex Pa_GetHostApiCount( void );
|#
(define-semi-checked pa-get-host-api-count
  (get-ffi-obj "Pa_GetHostApiCount"
               libportaudio
               (_fun -> _pa-host-api-index)))

;; import the function with a plain int return, to simplify
;; checking to see whether things have already been initialized.
(define pa-get-host-api-count/raw
  (get-ffi-obj "Pa_GetHostApiCount"
               libportaudio
               (_fun -> _int)))
#|


/** Retrieve the index of the default host API. The default host API will be
 the lowest common denominator host API on the current platform and is
 unlikely to provide the best performance.

 @return A non-negative value ranging from 0 to (Pa_GetHostApiCount()-1)
 indicating the default host API index or, a PaErrorCode (which are always
 negative) if PortAudio is not initialized or an error is encountered.
*/
PaHostApiIndex Pa_GetDefaultHostApi( void );
|#
(define-semi-checked pa-get-default-host-api
  (get-ffi-obj "Pa_GetDefaultHostApi"
               libportaudio
               (_fun -> _pa-host-api-index)))
#|

/** Unchanging unique identifiers for each supported host API. This type
 is used in the PaHostApiInfo structure. The values are guaranteed to be
 unique and to never change, thus allowing code to be written that
 conditionally uses host API specific extensions.

 New type ids will be allocated when support for a host API reaches
 "public alpha" status, prior to that developers should use the
 paInDevelopment type id.

 @see PaHostApiInfo
*/
typedef enum PaHostApiTypeId
{
    paInDevelopment=0, /* use while developing support for a new host API */
    paDirectSound=1,
    paMME=2,
    paASIO=3,
    paSoundManager=4,
    paCoreAudio=5,
    paOSS=7,
    paALSA=8,
    paAL=9,
    paBeOS=10,
    paWDMKS=11,
    paJACK=12,
    paWASAPI=13,
    paAudioScienceHPI=14
} PaHostApiTypeId;
|#
(define _pa-host-api-type-id
  (_enum
   '(paInDevelopment = 0 ;;/* use while developing support for a new host API */
     paDirectSound = 1
     paMME = 2
     paASIO = 3
     paSoundManager = 4
     paCoreAudio = 5
     paOSS = 7
     paALSA = 8
     paAL = 9
     paBeOS = 10
     paWDMKS = 11
     paJACK = 12
     paWASAPI = 13
     paAudioScienceHPI = 14
     )))
#|


/** A structure containing information about a particular host API. */

typedef struct PaHostApiInfo
{
    /** this is struct version 1 */
    int structVersion;
    /** The well known unique identifier of this host API @see PaHostApiTypeId */
    PaHostApiTypeId type;
    /** A textual description of the host API for display on user interfaces. */
    const char *name;

    /**  The number of devices belonging to this host API. This field may be
     used in conjunction with Pa_HostApiDeviceIndexToDeviceIndex() to enumerate
     all devices for this host API.
     @see Pa_HostApiDeviceIndexToDeviceIndex
    */
    int deviceCount;

    /** The default input device for this host API. The value will be a
     device index ranging from 0 to (Pa_GetDeviceCount()-1), or paNoDevice
     if no default input device is available.
    */
    PaDeviceIndex defaultInputDevice;

    /** The default output device for this host API. The value will be a
     device index ranging from 0 to (Pa_GetDeviceCount()-1), or paNoDevice
     if no default output device is available.
    */
    PaDeviceIndex defaultOutputDevice;
    
} PaHostApiInfo;
|#
(define-cstruct _pa-host-api-info
  ([struct-version _int]
   [type _pa-host-api-type-id]
   [name _string]
   [device-count _int]
   [default-input-device _pa-device-index]
   [default-output-device _pa-device-index]))

#|


/** Retrieve a pointer to a structure containing information about a specific
 host Api.

 @param hostApi A valid host API index ranging from 0 to (Pa_GetHostApiCount()-1)

 @return A pointer to an immutable PaHostApiInfo structure describing
 a specific host API. If the hostApi parameter is out of range or an error
 is encountered, the function returns NULL.

 The returned structure is owned by the PortAudio implementation and must not
 be manipulated or freed. The pointer is only guaranteed to be valid between
 calls to Pa_Initialize() and Pa_Terminate().
*/
const PaHostApiInfo * Pa_GetHostApiInfo( PaHostApiIndex hostApi );
|#
(define (pa-get-host-api-info index)
  (define num-indexes (pa-get-host-api-count))
  (unless (and (integer? index) (<= 0 index (sub1 num-indexes)))
    (raise-type-error 'pa-get-host-api-info 
                      (format "number in [0,~s] (legal API index)" (sub1 num-indexes))
                      0
                      index))
  (pa-get-host-api-info/core index))

(define pa-get-host-api-info/core
  (get-ffi-obj "Pa_GetHostApiInfo"
               libportaudio
               (_fun _pa-host-api-index -> _pa-host-api-info-pointer)))

;; enumerate the symbols associated with the supported APIs
(define (pa-get-all-api-ids)
  (for/list ([i (in-range (pa-get-host-api-count))])
    (pa-host-api-info-type (pa-get-host-api-info i))))
#|


/** Convert a static host API unique identifier, into a runtime
 host API index.

 @param type A unique host API identifier belonging to the PaHostApiTypeId
 enumeration.

 @return A valid PaHostApiIndex ranging from 0 to (Pa_GetHostApiCount()-1) or,
 a PaErrorCode (which are always negative) if PortAudio is not initialized
 or an error is encountered.
 
 The paHostApiNotFound error code indicates that the host API specified by the
 type parameter is not available.

 @see PaHostApiTypeId
*/
PaHostApiIndex Pa_HostApiTypeIdToHostApiIndex( PaHostApiTypeId type );
|#
(define pa-host-api-type-id-to-host-api-index
  (get-ffi-obj "Pa_HostApiTypeIdToHostApiIndex"
               libportaudio
               (_fun _pa-host-api-type-id -> _pa-host-api-index)))

#|


/** Convert a host-API-specific device index to standard PortAudio device index.
 This function may be used in conjunction with the deviceCount field of
 PaHostApiInfo to enumerate all devices for the specified host API.

 @param hostApi A valid host API index ranging from 0 to (Pa_GetHostApiCount()-1)

 @param hostApiDeviceIndex A valid per-host device index in the range
 0 to (Pa_GetHostApiInfo(hostApi)->deviceCount-1)

 @return A non-negative PaDeviceIndex ranging from 0 to (Pa_GetDeviceCount()-1)
 or, a PaErrorCode (which are always negative) if PortAudio is not initialized
 or an error is encountered.

 A paInvalidHostApi error code indicates that the host API index specified by
 the hostApi parameter is out of range.

 A paInvalidDevice error code indicates that the hostApiDeviceIndex parameter
 is out of range.
 
 @see PaHostApiInfo
*/
PaDeviceIndex Pa_HostApiDeviceIndexToDeviceIndex( PaHostApiIndex hostApi,
        int hostApiDeviceIndex );



/** Structure used to return information about a host error condition.
*/
typedef struct PaHostErrorInfo{
    PaHostApiTypeId hostApiType;    /**< the host API which returned the error code */
    long errorCode;                 /**< the error code returned */
    const char *errorText;          /**< a textual description of the error if available, otherwise a zero-length string */
}PaHostErrorInfo;
|#
(define-cstruct _pa-host-error-info
  ([host-api-type _pa-host-api-type-id]
   [error-code _long]
   [error-text _string]))
#|


/** Return information about the last host error encountered. The error
 information returned by Pa_GetLastHostErrorInfo() will never be modified
 asynchronously by errors occurring in other PortAudio owned threads
 (such as the thread that manages the stream callback.)

 This function is provided as a last resort, primarily to enhance debugging
 by providing clients with access to all available error information.

 @return A pointer to an immutable structure constraining information about
 the host error. The values in this structure will only be valid if a
 PortAudio function has previously returned the paUnanticipatedHostError
 error code.
*/
const PaHostErrorInfo* Pa_GetLastHostErrorInfo( void );
|#
(define pa-get-last-host-error-info
  (get-ffi-obj "Pa_GetLastHostErrorInfo"
               libportaudio
               (_fun -> _pa-host-error-info-pointer)))
#|


/* Device enumeration and capabilities */

/** Retrieve the number of available devices. The number of available devices
 may be zero.

 @return A non-negative value indicating the number of available devices or,
 a PaErrorCode (which are always negative) if PortAudio is not initialized
 or an error is encountered.
*/
PaDeviceIndex Pa_GetDeviceCount( void );
|#
(define-semi-checked pa-get-device-count
  (get-ffi-obj "Pa_GetDeviceCount"
               libportaudio
               (_fun -> _pa-device-index)))
#|


/** Retrieve the index of the default input device. The result can be
 used in the inputDevice parameter to Pa_OpenStream().

 @return The default input device index for the default host API, or paNoDevice
 if no default input device is available or an error was encountered.
*/
PaDeviceIndex Pa_GetDefaultInputDevice( void );
|#
(define pa-get-default-input-device
  (get-ffi-obj "Pa_GetDefaultInputDevice"
               libportaudio
               (_fun -> (index : _pa-device-index)
                     -> (match index
                          [-1 (error 'pa-get-default-input-device
                                                "no devices available")]
                          [-2
                           (error 'pa-get-default-input-device
                                  "use host-api-specific device specification")]
                          [other index]))))

#|


/** Retrieve the index of the default output device. The result can be
 used in the outputDevice parameter to Pa_OpenStream().

 @return The default output device index for the default host API, or paNoDevice
 if no default output device is available or an error was encountered.

 @note
 On the PC, the user can specify a default device by
 setting an environment variable. For example, to use device #1.
<pre>
 set PA_RECOMMENDED_OUTPUT_DEVICE=1
</pre>
 The user should first determine the available device ids by using
 the supplied application "pa_devs".
*/
PaDeviceIndex Pa_GetDefaultOutputDevice( void );
|#
(define pa-get-default-output-device
  (get-ffi-obj "Pa_GetDefaultOutputDevice"
               libportaudio
               (_fun -> (index : _pa-device-index)
                     -> (match index
                          [-1 (error 'pa-get-default-input-device
                                                "no devices available")]
                          [-2
                           (error 'pa-get-default-input-device
                                  "use host-api-specific device specification")]
                          [other index]))))
#|


/** The type used to represent monotonic time in seconds. PaTime is 
 used for the fields of the PaStreamCallbackTimeInfo argument to the 
 PaStreamCallback and as the result of Pa_GetStreamTime().

 PaTime values have unspecified origin.
     
 @see PaStreamCallback, PaStreamCallbackTimeInfo, Pa_GetStreamTime
*/
typedef double PaTime;
|#
(define _pa-time _double)


#|
/** A type used to specify one or more sample formats. Each value indicates
 a possible format for sound data passed to and from the stream callback,
 Pa_ReadStream and Pa_WriteStream.

 The standard formats paFloat32, paInt16, paInt32, paInt24, paInt8
 and aUInt8 are usually implemented by all implementations.

 The floating point representation (paFloat32) uses +1.0 and -1.0 as the
 maximum and minimum respectively.

 paUInt8 is an unsigned 8 bit format where 128 is considered "ground"

 The paNonInterleaved flag indicates that audio data is passed as an array 
 of pointers to separate buffers, one buffer for each channel. Usually,
 when this flag is not used, audio data is passed as a single buffer with
 all channels interleaved.

 @see Pa_OpenStream, Pa_OpenDefaultStream, PaDeviceInfo
 @see paFloat32, paInt16, paInt32, paInt24, paInt8
 @see paUInt8, paCustomFormat, paNonInterleaved
*/
typedef unsigned long PaSampleFormat;


#define paFloat32        ((PaSampleFormat) 0x00000001) /**< @see PaSampleFormat */
#define paInt32          ((PaSampleFormat) 0x00000002) /**< @see PaSampleFormat */
#define paInt24          ((PaSampleFormat) 0x00000004) /**< Packed 24 bit format. @see PaSampleFormat */
#define paInt16          ((PaSampleFormat) 0x00000008) /**< @see PaSampleFormat */
#define paInt8           ((PaSampleFormat) 0x00000010) /**< @see PaSampleFormat */
#define paUInt8          ((PaSampleFormat) 0x00000020) /**< @see PaSampleFormat */
#define paCustomFormat   ((PaSampleFormat) 0x00010000) /**< @see PaSampleFormat */

#define paNonInterleaved ((PaSampleFormat) 0x80000000) /**< @see PaSampleFormat */
|#

(define _pa-sample-format
  (_bitmask
   '(paFloat32        = #x00000001
     paInt32          = #x00000002
     paInt24          = #x00000004
     paInt16          = #x00000008
     paInt8           = #x00000010
     paUInt8          = #x00000020
     paCustomFormat   = #x00010000
     
     paNonInterleaved = #x80000000)
   _ulong))


(define _pa-stream-pointer _pointer)

#|

/** A structure providing information and capabilities of PortAudio devices.
 Devices may support input, output or both input and output.
*/
typedef struct PaDeviceInfo
{
    int structVersion;  /* this is struct version 2 */
    const char *name;
    PaHostApiIndex hostApi; /* note this is a host API index, not a type id*/
    
    int maxInputChannels;
    int maxOutputChannels;

    /* Default latency values for interactive performance. */
    PaTime defaultLowInputLatency;
    PaTime defaultLowOutputLatency;
    /* Default latency values for robust non-interactive applications (eg. playing sound files). */
    PaTime defaultHighInputLatency;
    PaTime defaultHighOutputLatency;

    double defaultSampleRate;
} PaDeviceInfo;
|#
(define-cstruct _pa-device-info
  ([struct-version      _int]
   [name                _string]
   [host-api            _pa-host-api-index]
   [max-input-channels  _int]
   [max-output-channels _int]
   [default-low-input-latency   _pa-time]
   [default-low-output-latency  _pa-time]
   [default-high-input-latency  _pa-time]
   [default-high-output-latency _pa-time]))

#|

/** Retrieve a pointer to a PaDeviceInfo structure containing information
 about the specified device.
 @return A pointer to an immutable PaDeviceInfo structure. If the device
 parameter is out of range the function returns NULL.

 @param device A valid device index in the range 0 to (Pa_GetDeviceCount()-1)

 @note PortAudio manages the memory referenced by the returned pointer,
 the client must not manipulate or free the memory. The pointer is only
 guaranteed to be valid between calls to Pa_Initialize() and Pa_Terminate().

 @see PaDeviceInfo, PaDeviceIndex
*/
const PaDeviceInfo* Pa_GetDeviceInfo( PaDeviceIndex device );
|#
(define pa-get-device-info
  (get-ffi-obj "Pa_GetDeviceInfo"
               libportaudio
               (_fun _pa-device-index -> _pa-device-info-pointer)))



#|


/** Parameters for one direction (input or output) of a stream.
*/
typedef struct PaStreamParameters
{
    /** A valid device index in the range 0 to (Pa_GetDeviceCount()-1)
     specifying the device to be used or the special constant
     paUseHostApiSpecificDeviceSpecification which indicates that the actual
     device(s) to use are specified in hostApiSpecificStreamInfo.
     This field must not be set to paNoDevice.
    */
    PaDeviceIndex device;
    
    /** The number of channels of sound to be delivered to the
     stream callback or accessed by Pa_ReadStream() or Pa_WriteStream().
     It can range from 1 to the value of maxInputChannels in the
     PaDeviceInfo record for the device specified by the device parameter.
    */
    int channelCount;

    /** The sample format of the buffer provided to the stream callback,
     a_ReadStream() or Pa_WriteStream(). It may be any of the formats described
     by the PaSampleFormat enumeration.
    */
    PaSampleFormat sampleFormat;

    /** The desired latency in seconds. Where practical, implementations should
     configure their latency based on these parameters, otherwise they may
     choose the closest viable latency instead. Unless the suggested latency
     is greater than the absolute upper limit for the device implementations
     should round the suggestedLatency up to the next practical value - ie to
     provide an equal or higher latency than suggestedLatency wherever possible.
     Actual latency values for an open stream may be retrieved using the
     inputLatency and outputLatency fields of the PaStreamInfo structure
     returned by Pa_GetStreamInfo().
     @see default*Latency in PaDeviceInfo, *Latency in PaStreamInfo
    */
    PaTime suggestedLatency;

    /** An optional pointer to a host api specific data structure
     containing additional information for device setup and/or stream processing.
     hostApiSpecificStreamInfo is never required for correct operation,
     if not used it should be set to NULL.
    */
    void *hostApiSpecificStreamInfo;

} PaStreamParameters;

|#


(define-cstruct _pa-stream-parameters
  ([device                        _pa-device-index]
   [channel-count                 _int]
   [sample-format                 _pa-sample-format]
   [suggested-latency             _pa-time]
   [host-api-specific-stream-info _pointer]))

#|

/** Return code for Pa_IsFormatSupported indicating success. */
#define paFormatIsSupported (0)

/** Determine whether it would be possible to open a stream with the specified
 parameters.

 @param inputParameters A structure that describes the input parameters used to
 open a stream. The suggestedLatency field is ignored. See PaStreamParameters
 for a description of these parameters. inputParameters must be NULL for
 output-only streams.

 @param outputParameters A structure that describes the output parameters used
 to open a stream. The suggestedLatency field is ignored. See PaStreamParameters
 for a description of these parameters. outputParameters must be NULL for
 input-only streams.

 @param sampleRate The required sampleRate. For full-duplex streams it is the
 sample rate for both input and output

 @return Returns 0 if the format is supported, and an error code indicating why
 the format is not supported otherwise. The constant paFormatIsSupported is
 provided to compare with the return value for success.

 @see paFormatIsSupported, PaStreamParameters
*/
PaError Pa_IsFormatSupported( const PaStreamParameters *inputParameters,
                              const PaStreamParameters *outputParameters,
                              double sampleRate );

|#
(define pa-is-format-supported
  (get-ffi-obj "Pa_IsFormatSupported"
               libportaudio
               (_fun _pa-stream-parameters-pointer/null
                     _pa-stream-parameters-pointer/null
                     _double
                     -> _pa-error)))

#|


/* Streaming types and functions */


/**
 A single PaStream can provide multiple channels of real-time
 streaming audio input and output to a client application. A stream
 provides access to audio hardware represented by one or more
 PaDevices. Depending on the underlying Host API, it may be possible 
 to open multiple streams using the same device, however this behavior 
 is implementation defined. Portable applications should assume that 
 a PaDevice may be simultaneously used by at most one PaStream.

 Pointers to PaStream objects are passed between PortAudio functions that
 operate on streams.

 @see Pa_OpenStream, Pa_OpenDefaultStream, Pa_OpenDefaultStream, Pa_CloseStream,
 Pa_StartStream, Pa_StopStream, Pa_AbortStream, Pa_IsStreamActive,
 Pa_GetStreamTime, Pa_GetStreamCpuLoad

*/
typedef void PaStream;


/** Can be passed as the framesPerBuffer parameter to Pa_OpenStream()
 or Pa_OpenDefaultStream() to indicate that the stream callback will
 accept buffers of any size.
*/
#define paFramesPerBufferUnspecified  (0)


/** Flags used to control the behavior of a stream. They are passed as
 parameters to Pa_OpenStream or Pa_OpenDefaultStream. Multiple flags may be
 ORed together.

 @see Pa_OpenStream, Pa_OpenDefaultStream
 @see paNoFlag, paClipOff, paDitherOff, paNeverDropInput,
  paPrimeOutputBuffersUsingStreamCallback, paPlatformSpecificFlags
*/
typedef unsigned long PaStreamFlags;

/** @see PaStreamFlags */
#define   paNoFlag          ((PaStreamFlags) 0)

/** Disable default clipping of out of range samples.
 @see PaStreamFlags
*/
#define   paClipOff         ((PaStreamFlags) 0x00000001)

/** Disable default dithering.
 @see PaStreamFlags
*/
#define   paDitherOff       ((PaStreamFlags) 0x00000002)

/** Flag requests that where possible a full duplex stream will not discard
 overflowed input samples without calling the stream callback. This flag is
 only valid for full duplex callback streams and only when used in combination
 with the paFramesPerBufferUnspecified (0) framesPerBuffer parameter. Using
 this flag incorrectly results in a paInvalidFlag error being returned from
 Pa_OpenStream and Pa_OpenDefaultStream.

 @see PaStreamFlags, paFramesPerBufferUnspecified
*/
#define   paNeverDropInput  ((PaStreamFlags) 0x00000004)

/** Call the stream callback to fill initial output buffers, rather than the
 default behavior of priming the buffers with zeros (silence). This flag has
 no effect for input-only and blocking read/write streams.
 
 @see PaStreamFlags
*/
#define   paPrimeOutputBuffersUsingStreamCallback ((PaStreamFlags) 0x00000008)

/** A mask specifying the platform specific bits.
 @see PaStreamFlags
*/
#define   paPlatformSpecificFlags ((PaStreamFlags)0xFFFF0000)
|#


;; *** UNTESTED ***:

(define _pa-stream-flags
  (_bitmask
   '(pa-no-flag                 = #x00000000
     pa-clip-off                = #x00000001
     pa-dither-off              = #x00000002
     pa-never-drop-input        = #x00000004
     pa-prime-output-buffers-using-stream-callback = #x00000008
     pa-platform-specific-flags = #xFFFF0000)
   _ulong))


#|
/**
 Timing information for the buffers passed to the stream callback.
*/
typedef struct PaStreamCallbackTimeInfo{
    PaTime inputBufferAdcTime;
    PaTime currentTime;
    PaTime outputBufferDacTime;
} PaStreamCallbackTimeInfo;
|#


;; *** UNTESTED ***:

(define-cstruct _pa-stream-callback-time-info
  ([input-buffer-adc-time  _pa-time]
   [current-time           _pa-time]
   [output-buffer-dac-time _pa-time]))
#|
/**
 Flag bit constants for the statusFlags to PaStreamCallback.

 @see paInputUnderflow, paInputOverflow, paOutputUnderflow, paOutputOverflow,
 paPrimingOutput
*/
typedef unsigned long PaStreamCallbackFlags;

/** In a stream opened with paFramesPerBufferUnspecified, indicates that
 input data is all silence (zeros) because no real data is available. In a
 stream opened without paFramesPerBufferUnspecified, it indicates that one or
 more zero samples have been inserted into the input buffer to compensate
 for an input underflow.
 @see PaStreamCallbackFlags
*/
#define paInputUnderflow   ((PaStreamCallbackFlags) 0x00000001)

/** In a stream opened with paFramesPerBufferUnspecified, indicates that data
 prior to the first sample of the input buffer was discarded due to an
 overflow, possibly because the stream callback is using too much CPU time.
 Otherwise indicates that data prior to one or more samples in the
 input buffer was discarded.
 @see PaStreamCallbackFlags
*/
#define paInputOverflow    ((PaStreamCallbackFlags) 0x00000002)

/** Indicates that output data (or a gap) was inserted, possibly because the
 stream callback is using too much CPU time.
 @see PaStreamCallbackFlags
*/
#define paOutputUnderflow  ((PaStreamCallbackFlags) 0x00000004)

/** Indicates that output data will be discarded because no room is available.
 @see PaStreamCallbackFlags
*/
#define paOutputOverflow   ((PaStreamCallbackFlags) 0x00000008)

/** Some of all of the output data will be used to prime the stream, input
 data may be zero.
 @see PaStreamCallbackFlags
*/
#define paPrimingOutput    ((PaStreamCallbackFlags) 0x00000010)

|#


;; *** UNTESTED ***:

(define _pa-stream-callback-flags
  (_bitmask
   '(pa-input-underflow  = #x00000001
     pa-input-overflow   = #x00000002
     pa-output-underflow = #x00000004
     pa-output-overflow  = #x00000008
     pa-priming-output   = #x00000010)
   _ulong))

#|

/**
 Allowable return values for the PaStreamCallback.
 @see PaStreamCallback
*/
typedef enum PaStreamCallbackResult
{
    paContinue=0,
    paComplete=1,
    paAbort=2
} PaStreamCallbackResult;

|#

(define _pa-stream-callback-result
  (_enum
   '(pa-continue = 0
     pa-complete = 1
     pa-abort    = 2)))

#|

/**
 Functions of type PaStreamCallback are implemented by PortAudio clients.
 They consume, process or generate audio in response to requests from an
 active PortAudio stream.
     
 @param input and @param output are either arrays of interleaved samples or;
 if non-interleaved samples were requested using the paNonInterleaved sample 
 format flag, an array of buffer pointers, one non-interleaved buffer for 
 each channel.

 The format, packing and number of channels used by the buffers are
 determined by parameters to Pa_OpenStream().
     
 @param frameCount The number of sample frames to be processed by
 the stream callback.

 @param timeInfo The time in seconds when the first sample of the input
 buffer was received at the audio input, the time in seconds when the first
 sample of the output buffer will begin being played at the audio output, and
 the time in seconds when the stream callback was called.
 See also Pa_GetStreamTime()

 @param statusFlags Flags indicating whether input and/or output buffers
 have been inserted or will be dropped to overcome underflow or overflow
 conditions.

 @param userData The value of a user supplied pointer passed to
 Pa_OpenStream() intended for storing synthesis data etc.

 @return
 The stream callback should return one of the values in the
 PaStreamCallbackResult enumeration. To ensure that the callback continues
 to be called, it should return paContinue (0). Either paComplete or paAbort
 can be returned to finish stream processing, after either of these values is
 returned the callback will not be called again. If paAbort is returned the
 stream will finish as soon as possible. If paComplete is returned, the stream
 will continue until all buffers generated by the callback have been played.
 This may be useful in applications such as soundfile players where a specific
 duration of output is required. However, it is not necessary to utilize this
 mechanism as Pa_StopStream(), Pa_AbortStream() or Pa_CloseStream() can also
 be used to stop the stream. The callback must always fill the entire output
 buffer irrespective of its return value.

 @see Pa_OpenStream, Pa_OpenDefaultStream

 @note With the exception of Pa_GetStreamCpuLoad() it is not permissible to call
 PortAudio API functions from within the stream callback.
*/
typedef int PaStreamCallback(
    const void *input, void *output,
    unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData );

|#

;;instead, just define it as an opaque pointer:
(define-cpointer-type _pa-stream-callback)


#|
/** Opens a stream for either input, output or both.
     
 @param stream The address of a PaStream pointer which will receive
 a pointer to the newly opened stream.
     
 @param inputParameters A structure that describes the input parameters used by
 the opened stream. See PaStreamParameters for a description of these parameters.
 inputParameters must be NULL for output-only streams.

 @param outputParameters A structure that describes the output parameters used by
 the opened stream. See PaStreamParameters for a description of these parameters.
 outputParameters must be NULL for input-only streams.
 
 @param sampleRate The desired sampleRate. For full-duplex streams it is the
 sample rate for both input and output
     
 @param framesPerBuffer The number of frames passed to the stream callback
 function, or the preferred block granularity for a blocking read/write stream.
 The special value paFramesPerBufferUnspecified (0) may be used to request that
 the stream callback will receive an optimal (and possibly varying) number of
 frames based on host requirements and the requested latency settings.
 Note: With some host APIs, the use of non-zero framesPerBuffer for a callback
 stream may introduce an additional layer of buffering which could introduce
 additional latency. PortAudio guarantees that the additional latency
 will be kept to the theoretical minimum however, it is strongly recommended
 that a non-zero framesPerBuffer value only be used when your algorithm
 requires a fixed number of frames per stream callback.
 
 @param streamFlags Flags which modify the behavior of the streaming process.
 This parameter may contain a combination of flags ORed together. Some flags may
 only be relevant to certain buffer formats.
     
 @param streamCallback A pointer to a client supplied function that is responsible
 for processing and filling input and output buffers. If this parameter is NULL
 the stream will be opened in 'blocking read/write' mode. In blocking mode,
 the client can receive sample data using Pa_ReadStream and write sample data
 using Pa_WriteStream, the number of samples that may be read or written
 without blocking is returned by Pa_GetStreamReadAvailable and
 Pa_GetStreamWriteAvailable respectively.

 @param userData A client supplied pointer which is passed to the stream callback
 function. It could for example, contain a pointer to instance data necessary
 for processing the audio buffers. This parameter is ignored if streamCallback
 is NULL.
     
 @return
 Upon success Pa_OpenStream() returns paNoError and places a pointer to a
 valid PaStream in the stream argument. The stream is inactive (stopped).
 If a call to Pa_OpenStream() fails, a non-zero error code is returned (see
 PaError for possible error codes) and the value of stream is invalid.

 @see PaStreamParameters, PaStreamCallback, Pa_ReadStream, Pa_WriteStream,
 Pa_GetStreamReadAvailable, Pa_GetStreamWriteAvailable
*/
PaError Pa_OpenStream( PaStream** stream,
                       const PaStreamParameters *inputParameters,
                       const PaStreamParameters *outputParameters,
                       double sampleRate,
                       unsigned long framesPerBuffer,
                       PaStreamFlags streamFlags,
                       PaStreamCallback *streamCallback,
                       void *userData );

|#

(define pa-open-stream
  (get-ffi-obj "Pa_OpenStream"
               libportaudio
               (_fun (result : (_ptr o _pa-stream-pointer)) ;; stream
                     _pa-stream-parameters-pointer/null ;; inputParameters
                     _pa-stream-parameters-pointer/null ;; outputParameters
                     _double ;; sampleRate
                     _ulong ;; framesPerBuffer
                     _pa-stream-flags ;; streamFlags
                     _pa-stream-callback ;; callback (ptr to C fun)
                     _pointer ;; userData
                     -> (err : _pa-error)
                     -> (match err
                          ['paNoError
                           (begin (add-managed result 
                                               close-stream-callback)
                                  result)]
                          [other (error 'pa-open-stream "~a" 
                                        (pa-get-error-text err))]))))


#| 
/** A simplified version of Pa_OpenStream() that opens the default input
 and/or output devices.

 @param stream The address of a PaStream pointer which will receive
 a pointer to the newly opened stream.
 
 @param numInputChannels  The number of channels of sound that will be supplied
 to the stream callback or returned by Pa_ReadStream. It can range from 1 to
 the value of maxInputChannels in the PaDeviceInfo record for the default input
 device. If 0 the stream is opened as an output-only stream.

 @param numOutputChannels The number of channels of sound to be delivered to the
 stream callback or passed to Pa_WriteStream. It can range from 1 to the value
 of maxOutputChannels in the PaDeviceInfo record for the default output device.
 If 0 the stream is opened as an output-only stream.

 @param sampleFormat The sample format of both the input and output buffers
 provided to the callback or passed to and from Pa_ReadStream and Pa_WriteStream.
 sampleFormat may be any of the formats described by the PaSampleFormat
 enumeration.
 
 @param sampleRate Same as Pa_OpenStream parameter of the same name.
 @param framesPerBuffer Same as Pa_OpenStream parameter of the same name.
 @param streamCallback Same as Pa_OpenStream parameter of the same name.
 @param userData Same as Pa_OpenStream parameter of the same name.

 @return As for Pa_OpenStream

 @see Pa_OpenStream, PaStreamCallback
*/
PaError Pa_OpenDefaultStream( PaStream** stream,
                              int numInputChannels,
                              int numOutputChannels,
                              PaSampleFormat sampleFormat,
                              double sampleRate,
                              unsigned long framesPerBuffer,
                              PaStreamCallback *streamCallback,
                              void *userData );
|#
(define pa-open-default-stream
  (get-ffi-obj "Pa_OpenDefaultStream"
               libportaudio
               (_fun (result : (_ptr o _pa-stream-pointer)) ;; stream
                     _int ;; numInputChannels
                     _int ;; numOutputChannels
                     _pa-sample-format ;; sampleFormat
                     _double ;; sampleRate
                     _ulong ;; framesPerBuffer
                     _pa-stream-callback ;; callback
                     _pointer ;; userData?
                     -> (err : _pa-error)
                     -> (match err
                          ['paNoError  
                           (begin (add-managed result 
                                               close-stream-callback)
                                  result)]
                          [other (error 'pa-open-default-stream "~a" 
                                        (pa-get-error-text err))]))))


(define close-stream-callback
  (ffi-callback (lambda (p _)
                  (pa-maybe-stop-stream p))
                (list _racket _pointer)
                _void))

#|


/** Closes an audio stream. If the audio stream is active it
 discards any pending buffers as if Pa_AbortStream() had been called.
*/
PaError Pa_CloseStream( PaStream *stream );
|#
(define (pa-close-stream stream)
  ;; unregister with the custodian
  (remove-managed stream)
  (pa-close-stream/raw stream))

(define-checked pa-close-stream/raw
  (get-ffi-obj "Pa_CloseStream"
               libportaudio
               (_fun _pa-stream-pointer -> _pa-error)))

#|

/** Functions of type PaStreamFinishedCallback are implemented by PortAudio 
 clients. They can be registered with a stream using the Pa_SetStreamFinishedCallback
 function. Once registered they are called when the stream becomes inactive
 (ie once a call to Pa_StopStream() will not block).
 A stream will become inactive after the stream callback returns non-zero,
 or when Pa_StopStream or Pa_AbortStream is called. For a stream providing audio
 output, if the stream callback returns paComplete, or Pa_StopStream is called,
 the stream finished callback will not be called until all generated sample data
 has been played.
 
 @param userData The userData parameter supplied to Pa_OpenStream()

 @see Pa_SetStreamFinishedCallback
*/
typedef void PaStreamFinishedCallback( void *userData );
|#
;; not porting this to scheme; it's a better idea just to keep it in C.
(define-cpointer-type _pa-stream-finished-callback)
#|


/** Register a stream finished callback function which will be called when the 
 stream becomes inactive. See the description of PaStreamFinishedCallback for 
 further details about when the callback will be called.

 @param stream a pointer to a PaStream that is in the stopped state - if the
 stream is not stopped, the stream's finished callback will remain unchanged 
 and an error code will be returned.

 @param streamFinishedCallback a pointer to a function with the same signature
 as PaStreamFinishedCallback, that will be called when the stream becomes
 inactive. Passing NULL for this parameter will un-register a previously
 registered stream finished callback function.

 @return on success returns paNoError, otherwise an error code indicating the cause
 of the error.

 @see PaStreamFinishedCallback
*/
PaError Pa_SetStreamFinishedCallback( PaStream *stream, PaStreamFinishedCallback* streamFinishedCallback ); 
|#
(define-checked pa-set-stream-finished-callback
  (get-ffi-obj "Pa_SetStreamFinishedCallback"
               libportaudio
               (_fun _pa-stream-pointer _pa-stream-finished-callback -> _pa-error)))
#|

/** Commences audio processing.
*/
PaError Pa_StartStream( PaStream *stream );
|#
(define-checked pa-start-stream
  (get-ffi-obj "Pa_StartStream"
               libportaudio
               (_fun _pa-stream-pointer -> _pa-error)))

#|

/** Terminates audio processing. It waits until all pending
 audio buffers have been played before it returns.
*/
PaError Pa_StopStream( PaStream *stream );
|#
(define (pa-stop-stream stream)
  (remove-managed stream)
  (pa-stop-stream/raw stream))

(define-checked pa-stop-stream/raw
  (get-ffi-obj "Pa_StopStream"
               libportaudio
               (_fun _pa-stream-pointer -> _pa-error)))

#|

/** Terminates audio processing immediately without waiting for pending
 buffers to complete.
*/
PaError Pa_AbortStream( PaStream *stream );
|#

(define (pa-abort-stream stream)
  (remove-managed stream)
  (pa-abort-stream/raw stream))

(define-checked pa-abort-stream/raw
  (get-ffi-obj "Pa_AbortStream"
               libportaudio
               (_fun _pa-stream-pointer -> _pa-error)))



#|

/** Determine whether the stream is stopped.
 A stream is considered to be stopped prior to a successful call to
 Pa_StartStream and after a successful call to Pa_StopStream or Pa_AbortStream.
 If a stream callback returns a value other than paContinue the stream is NOT
 considered to be stopped.

 @return Returns one (1) when the stream is stopped, zero (0) when
 the stream is running or, a PaErrorCode (which are always negative) if
 PortAudio is not initialized or an error is encountered.

 @see Pa_StopStream, Pa_AbortStream, Pa_IsStreamActive
*/
PaError Pa_IsStreamStopped( PaStream *stream );
|#

;; untested:
(define pa-stream-stopped?
  (get-ffi-obj "Pa_IsStreamStopped"
               libportaudio
               (_fun _pa-stream-pointer 
                     -> (result : _int)
                     -> (cond [(= result 0) #f]
                              [(= result 1) #t]
                              [else (error (pa-get-error-text/int result))]))))

#|


/** Determine whether the stream is active.
 A stream is active after a successful call to Pa_StartStream(), until it
 becomes inactive either as a result of a call to Pa_StopStream() or
 Pa_AbortStream(), or as a result of a return value other than paContinue from
 the stream callback. In the latter case, the stream is considered inactive
 after the last buffer has finished playing.

 @return Returns one (1) when the stream is active (ie playing or recording
 audio), zero (0) when not playing or, a PaErrorCode (which are always negative)
 if PortAudio is not initialized or an error is encountered.

 @see Pa_StopStream, Pa_AbortStream, Pa_IsStreamStopped
*/
PaError Pa_IsStreamActive( PaStream *stream );
|#
(define pa-stream-active?
  (get-ffi-obj "Pa_IsStreamActive"
               libportaudio
               (_fun _pa-stream-pointer 
                     -> (result : _int)
                     -> (cond [(= result 0) #f]
                              [(= result 1) #t]
                              [else (error (pa-get-error-text/int result))]))))
#|


/** A structure containing unchanging information about an open stream.
 @see Pa_GetStreamInfo
*/

typedef struct PaStreamInfo
{
    /** this is struct version 1 */
    int structVersion;

    /** The input latency of the stream in seconds. This value provides the most
     accurate estimate of input latency available to the implementation. It may
     differ significantly from the suggestedLatency value passed to Pa_OpenStream().
     The value of this field will be zero (0.) for output-only streams.
     @see PaTime
    */
    PaTime inputLatency;

    /** The output latency of the stream in seconds. This value provides the most
     accurate estimate of output latency available to the implementation. It may
     differ significantly from the suggestedLatency value passed to Pa_OpenStream().
     The value of this field will be zero (0.) for input-only streams.
     @see PaTime
    */
    PaTime outputLatency;

    /** The sample rate of the stream in Hertz (samples per second). In cases
     where the hardware sample rate is inaccurate and PortAudio is aware of it,
     the value of this field may be different from the sampleRate parameter
     passed to Pa_OpenStream(). If information about the actual hardware sample
     rate is not available, this field will have the same value as the sampleRate
     parameter passed to Pa_OpenStream().
    */
    double sampleRate;
    
} PaStreamInfo;

|#

(define-cstruct _pa-stream-info
  ([struct-version _int]
   [input-latency  _pa-time]
   [output-latency _pa-time]
   [sample-rate    _double]))

#|

/** Retrieve a pointer to a PaStreamInfo structure containing information
 about the specified stream.
 @return A pointer to an immutable PaStreamInfo structure. If the stream
 parameter invalid, or an error is encountered, the function returns NULL.

 @param stream A pointer to an open stream previously created with Pa_OpenStream.

 @note PortAudio manages the memory referenced by the returned pointer,
 the client must not manipulate or free the memory. The pointer is only
 guaranteed to be valid until the specified stream is closed.

 @see PaStreamInfo
*/
const PaStreamInfo* Pa_GetStreamInfo( PaStream *stream );

|#
(define pa-get-stream-info
  (get-ffi-obj "Pa_GetStreamInfo"
               libportaudio
               (_fun _pa-stream-pointer -> _pa-stream-info-pointer)))
#|

/** Returns the current time in seconds for a stream according to the same clock used
 to generate callback PaStreamCallbackTimeInfo timestamps. The time values are
 monotonically increasing and have unspecified origin. 
 
 Pa_GetStreamTime returns valid time values for the entire life of the stream,
 from when the stream is opened until it is closed. Starting and stopping the stream
 does not affect the passage of time returned by Pa_GetStreamTime.

 This time may be used for synchronizing other events to the audio stream, for 
 example synchronizing audio to MIDI.
                                        
 @return The stream's current time in seconds, or 0 if an error occurred.

 @see PaTime, PaStreamCallback, PaStreamCallbackTimeInfo
*/
PaTime Pa_GetStreamTime( PaStream *stream );
|#

(define pa-get-stream-time
  (get-ffi-obj "Pa_GetStreamTime"
               libportaudio
               (_fun _pa-stream-pointer 
                     -> (result : _pa-time)
                     -> (cond [(= 0.0 result) 
                               (error 'pa-get-stream-time
                                      "unsuccessful")]
                              [else result]))))
#|


/** Retrieve CPU usage information for the specified stream.
 The "CPU Load" is a fraction of total CPU time consumed by a callback stream's
 audio processing routines including, but not limited to the client supplied
 stream callback. This function does not work with blocking read/write streams.

 This function may be called from the stream callback function or the
 application.
     
 @return
 A floating point value, typically between 0.0 and 1.0, where 1.0 indicates
 that the stream callback is consuming the maximum number of CPU cycles possible
 to maintain real-time operation. A value of 0.5 would imply that PortAudio and
 the stream callback was consuming roughly 50% of the available CPU time. The
 return value may exceed 1.0. A value of 0.0 will always be returned for a
 blocking read/write stream, or if an error occurs.
*/
double Pa_GetStreamCpuLoad( PaStream* stream );

|#

;; note that in the way this package is used by the built-in callbacks,
;; this number will be very low, because it only includes time spent 
;; copying (in C), not time spent generating samples (in Racket)

(define pa-get-stream-cpu-load
  (get-ffi-obj "Pa_GetStreamCpuLoad"
               libportaudio
               (_fun _pa-stream-pointer -> _double)))


#|

/** Read samples from an input stream. The function doesn't return until
 the entire buffer has been filled - this may involve waiting for the operating
 system to supply the data.

 @param stream A pointer to an open stream previously created with Pa_OpenStream.
 
 @param buffer A pointer to a buffer of sample frames. The buffer contains
 samples in the format specified by the inputParameters->sampleFormat field
 used to open the stream, and the number of channels specified by
 inputParameters->numChannels. If non-interleaved samples were requested using
 the paNonInterleaved sample format flag, buffer is a pointer to the first element 
 of an array of buffer pointers, one non-interleaved buffer for each channel.

 @param frames The number of frames to be read into buffer. This parameter
 is not constrained to a specific range, however high performance applications
 will want to match this parameter to the framesPerBuffer parameter used
 when opening the stream.

 @return On success PaNoError will be returned, or PaInputOverflowed if input
 data was discarded by PortAudio after the previous call and before this call.
*/
PaError Pa_ReadStream( PaStream* stream,
                       void *buffer,
                       unsigned long frames );
|#

;; *** UNTESTED ***:

(define-checked pa-read-stream
  (get-ffi-obj "Pa_ReadStream"
               libportaudio
               (_fun _pa-stream-pointer _pointer _ulong -> _pa-error)))

#|

/** Write samples to an output stream. This function doesn't return until the
 entire buffer has been consumed - this may involve waiting for the operating
 system to consume the data.

 @param stream A pointer to an open stream previously created with Pa_OpenStream.

 @param buffer A pointer to a buffer of sample frames. The buffer contains
 samples in the format specified by the outputParameters->sampleFormat field
 used to open the stream, and the number of channels specified by
 outputParameters->numChannels. If non-interleaved samples were requested using
 the paNonInterleaved sample format flag, buffer is a pointer to the first element 
 of an array of buffer pointers, one non-interleaved buffer for each channel.

 @param frames The number of frames to be written from buffer. This parameter
 is not constrained to a specific range, however high performance applications
 will want to match this parameter to the framesPerBuffer parameter used
 when opening the stream.

 @return On success PaNoError will be returned, or paOutputUnderflowed if
 additional output data was inserted after the previous call and before this
 call.
*/
PaError Pa_WriteStream( PaStream* stream,
                        const void *buffer,
                        unsigned long frames );
|#

(define-checked pa-write-stream
  (get-ffi-obj "Pa_WriteStream"
               libportaudio
               (_fun _pa-stream-pointer _pointer _ulong -> _pa-error)))

#|

/** Retrieve the number of frames that can be read from the stream without
 waiting.

 @return Returns a non-negative value representing the maximum number of frames
 that can be read from the stream without blocking or busy waiting or, a
 PaErrorCode (which are always negative) if PortAudio is not initialized or an
 error is encountered.
*/
signed long Pa_GetStreamReadAvailable( PaStream* stream );
|#

;; *** UNTESTED ***:

(define pa-get-stream-read-available
  (get-ffi-obj "Pa_GetStreamReadAvailable"
               libportaudio
               (_fun _pa-stream-pointer -> 
                     [err-or-result : _pa-error]
                     -> (cond [(< err-or-result 0) 
                               (error 'pa-get-stream-read-available "~a" 
                                      (pa-get-error-text err-or-result))]
                              [else err-or-result]))))

#|

/** Retrieve the number of frames that can be written to the stream without
 waiting.

 @return Returns a non-negative value representing the maximum number of frames
 that can be written to the stream without blocking or busy waiting or, a
 PaErrorCode (which are always negative) if PortAudio is not initialized or an
 error is encountered.
*/
signed long Pa_GetStreamWriteAvailable( PaStream* stream );

|#

(define pa-get-stream-write-available
  (get-ffi-obj "Pa_GetStreamWriteAvailable"
               libportaudio
               (_fun _pa-stream-pointer -> 
                     [err-or-result : _slong]
                     -> (cond [(< err-or-result 0) 
                               (error 'pa-get-stream-write-available "~a" 
                                      (pa-get-error-text err-or-result))]
                              [else err-or-result]))))

#|


/* Miscellaneous utilities */


/** Retrieve the size of a given sample format in bytes.

 @return The size in bytes of a single sample in the specified format,
 or paSampleFormatNotSupported if the format is not supported.
*/
PaError Pa_GetSampleSize( PaSampleFormat format );


/** Put the caller to sleep for at least 'msec' milliseconds. This function is
 provided only as a convenience for authors of portable code (such as the tests
 and examples in the PortAudio distribution.)

 The function may sleep longer than requested so don't rely on this for accurate
 musical timing.
*/
void Pa_Sleep( long msec );



#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* PORTAUDIO_H */

|#

;; SUPPORT:

;; import add-managed so that we can associate streams with custodians:
(define add-managed
 (get-ffi-obj "scheme_add_managed" #f
             (_fun (_pointer = #f) _racket _fpointer (_pointer = #f) (_int = 1) -> _pointer)))

;; import remove-managed so that we can detach streams from custodians:
(define remove-managed
 (get-ffi-obj "scheme_remove_managed" #f
             (_fun (_pointer = #f) _racket -> _void)))


;; WRAPPERS:

;; stop unless it's already been stopped.
(define (pa-maybe-stop-stream stream)
  (when (pa-stream-active? stream)
    (pa-stop-stream stream)))

;; initialize unless it's already been initialized.
(define (pa-maybe-initialize)
  (cond [(pa-initialized?) (void)]
        [else (pa-initialize)]))

;; has portaudio been initialized?
(define (pa-initialized?)
  (not (= (pa-get-host-api-count/raw) pa-not-initialized-error)))

;; terminate until pa-initialized returns false
(define (pa-terminate-completely)
  (let loop ([count terminate-absurd-threshold])
    (cond ([< count 0] (error 'pa-terminate-completely 
                              "terminated more than ~s times and initialized? still returns true."
                              terminate-absurd-threshold))
          [(pa-initialized?) (pa-terminate)
                             (loop (- count 1))]
          [else #t])))

;; the number of times to try terminating before giving up:
(define terminate-absurd-threshold 1000000)

;; UTILITIES

(define (stream-stats stream)
  (define stream-info (pa-get-stream-info stream))
  `((cpu-load ,(pa-get-stream-cpu-load stream))
    (input-latency ,(pa-stream-info-input-latency stream-info))
    (output-latency ,(pa-stream-info-output-latency stream-info))
    (sample-rate ,(pa-stream-info-sample-rate stream-info))))

;; provide information on all of the available devices
(define (available-devices-info)
  (for/list ([i (in-range (pa-get-device-count))])
    (pa-device-info->list (pa-get-device-info i))))

;; provide information on the default device
(define (default-device-info)
  (pa-device-info->list (pa-get-device-info 
                         (pa-get-default-output-device))))

;; device-name : natural -> string
;; return the name of the device with the given device number
(define (device-name i)
  (pa-device-info-name (pa-get-device-info i)))


;; reasonable-latency-output-devices : real -> (list-of natural?)
;; output devices with reasonable latency
(define (low-latency-output-devices latency)
  (for/list ([i (in-range (pa-get-device-count))]
        #:when (has-outputs? i)
        #:when (reasonable-latency? latency i))
    i))

;; has-outputs? : natural -> boolean
;; return true if the device has at least
;; two output channels
(define (has-outputs? i)
  (<= 2 (pa-device-info-max-output-channels (pa-get-device-info i))))

;; reasonable-latency? : natural real -> boolean
;; return true when the device has low latency
;; no greater than some threshold
(define (reasonable-latency? latency i)
  (<= (device-low-output-latency i) latency))

;; device-low-output-latency : natural -> real
;; return the low output latency of a device 
(define (device-low-output-latency i)
  (pa-device-info-default-low-output-latency (pa-get-device-info i)))

