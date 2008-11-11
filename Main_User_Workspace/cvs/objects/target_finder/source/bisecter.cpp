/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file bisecter.cpp
* \ingroup Objects
* \brief Bisecter class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include <cmath>
#include "util/logger/include/ilogger.h"
#include "target_finder/include/bisecter.h"
#include "target_finder/include/itarget.h"
#include "util/base/include/util.h"

using namespace std;

/*! \brief Value to use for undefined minimums, maximums and trial values.
* \return Undefined value.
*/
double Bisecter::undefined(){
    return -1;
}

/*!
 * \brief Construct the Bisecter.
 * \param aTarget The policy target.
 * \param aTolerance Solution tolerance.
 * \param aInitialValue Initial guess.
 * \param aMultiple Amount to adjust trial values by each iteration.
 * \param aYear Year which the bisecter is operating.
 * \param aLowerBound Lower bound.
 * \param aUpperBound Upper bound.
 */
Bisecter::Bisecter( const ITarget* aTarget,
                    const double aTolerance,
                    const double aMinimum,
                    const double aMaximum,
                    const double aInitialValue,
                    const double aMultiple,
                    const int aYear ) :
mTarget( aTarget ),
mTolerance( aTolerance ),
mMinimum( aMinimum ),
mMaximum( aMaximum ),
mLowerBound( Bisecter::undefined() ),
mUpperBound( Bisecter::undefined() ),
mInitialGuess( aInitialValue ),
mMultiple( aMultiple ),
mCurrentTrial( 0 ),
mYear( aYear ),
mIterations( 0 ){
    if( mInitialGuess != undefined() ){
        mCurrentTrial = mInitialGuess;
    }
    else if( mMinimum != undefined() ){
        mCurrentTrial = mMinimum;
    }
    else {
        mCurrentTrial = 0;
    }

    // Default multiple to 2.
    if( mMultiple == undefined() ){
        mMultiple = 2;
    }

    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    targetLog.setLevel( ILogger::DEBUG );
    targetLog << "Constructing a Bisecter. Min: " << mMinimum
        << " Max: " << mMaximum << " Initial trial: " << mCurrentTrial << endl;
}

/*! \brief Get the next trial value and check for solution.
* \details Checks if the PolicyTarget is solved and returns a pair representing
*          the next trial value and whether the PolicyTarget is solved.
* \return A pair representing the next trial value and whether the PolicyTarget
*         is solved.
*/
pair<double, bool> Bisecter::getNextValue() {
    // Get the status of the current trial.

    // If the target year is set to -1 this is means that the trial value should
    // be used as the target year. TODO: This is a little hackish.
    double targetYear = mYear != -1 ? mYear : mCurrentTrial;

    ITarget::TrialStatus currTrial = mTarget->getStatus( mTolerance, targetYear );

    ILogger& mainLog = ILogger::getLogger( "target_finder_log" );
    mainLog.setLevel( ILogger::WARNING );
    mainLog << "Current trial status is " << ITarget::toString( currTrial )
            << endl;
    
    SolvedState state = eUnsolved;

    switch( currTrial ){
        case ITarget::SOLVED:
            state = eSolved;
            break;
        case ITarget::LOW:
            // Set the upper bound to the current value.
            mUpperBound = mCurrentTrial;

            // If the lower bound is unknown set it to the minimum value.
            if( mLowerBound == Bisecter::undefined() ){
                // If the minimum was defined by the value use that as the true
                // lower bound. Otherwise use zero.
                if( mMinimum != undefined() ){
                    mLowerBound = mMinimum;
                }
                else {
                    mLowerBound = 0;
                }
            }

            // Check if the bracket is too small to contain any values.
            if( mUpperBound - mLowerBound < mTolerance ){
                state = eEmptyBracket;
            }
            // Check if the constraint is not binding. If the minimum was set by
            // the caller, use a minimum of the minimum plus a small number. If
            // the minimum was not set, use just the small number.
            else if( mCurrentTrial <
                ( mMinimum != undefined() ? mMinimum + mTolerance : mTolerance ) )
            {
                state = eLowerBoundReached;
            }
            else {
                // Calculate the new current trial. This will be ignored if the target is
                // solved.
                mCurrentTrial = mLowerBound + ( mUpperBound - mLowerBound ) / 2.0;
            }
            break;
        case ITarget::HIGH:
            // Set the lower bound to the current value.
            mLowerBound = mCurrentTrial;

            // If the upper bound is currently unknown set the trial to twice
            // it's current value or minimum if it is currently zero.
            if( mUpperBound == Bisecter::undefined() ){
                if( mCurrentTrial == mInitialGuess ){
                    mCurrentTrial = mInitialGuess;
                    // Clear the initial guess so we do not use it again.
                    // TODO: Also hackish.
                    mInitialGuess = undefined();
                }
                // If the maximum was defined by the value use that as the true
                // lower bound. Otherwise multiply upwards until the solution is bounded.
                else if( mMaximum != undefined() ){
                    mUpperBound = mMaximum;
                    mCurrentTrial = mLowerBound + ( mUpperBound - mLowerBound ) / 2.0;
                }
                else if( mCurrentTrial == 0 ){
                    mCurrentTrial = max( mMinimum, 1.0 );
                }
                else {
                    mCurrentTrial *= ( 1 + mMultiple );
                    // Don't allow the current trial to exceed the maximum.
                    if( mMaximum != undefined() ){
                        mCurrentTrial = min( mCurrentTrial, mMaximum );
                    }
                }
            }
            else {
                // Check if the bracket is too small to contain any values.
                if( mUpperBound - mLowerBound < mTolerance ){
                    state = eEmptyBracket;
                }
                // Check if the upper bound has already been reached.
                else if( mUpperBound - mCurrentTrial < mTolerance ){
                    state = eUpperBoundReached;
                }
                else {
                    // Calculate the new current trial.
                    mCurrentTrial = mLowerBound + ( mUpperBound - mLowerBound ) / 2.0;
                }
            }

            break;
    }

    // Increment the number of trials.
    ++mIterations;

    printState( state );

    assert( util::isValidNumber( mCurrentTrial ) );
    assert( mCurrentTrial >= 0 );
    return make_pair( mCurrentTrial, state != eUnsolved );
}

/*! \brief Print the current state of a bisection iteration.
* \param aState State enum.
*/
void Bisecter::printState( const SolvedState aState ) const {
    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    targetLog.setLevel( ILogger::DEBUG );
    switch( aState ){
        case eSolved:
            targetLog << "Found solution. ";
            break;
        case eUnsolved:
            targetLog << "Attempting to solve target. Iteration: "
                      << mIterations << " ";
            break;
        case eEmptyBracket:
            targetLog << "Failed to solve because the bracket width is empty. ";
            break;
        case eLowerBoundReached:
            targetLog << "Failed to solve because the lower bound was reached. ";
            break;
        case eUpperBoundReached:
            targetLog << "Failed to solve because the upper bound was reached. ";
            break;
    }

    targetLog << "The current trial is " << mCurrentTrial << ". Lower bound is "
              << mLowerBound << " and upper bound is " << mUpperBound << "." << endl;
}

/*! \brief Get the current number of iterations performed.
* \return The current number of iterations performed.
*/
unsigned int Bisecter::getIterations() const {
    return mIterations;
}