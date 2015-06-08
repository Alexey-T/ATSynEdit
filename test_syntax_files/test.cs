using System;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using EVControls;

namespace Burray
{
    public class HotWaterGuidedSchedulingScreen : CommonScreen
    {
        // External users should look here ONLY!
        #region Public Interface

        /// <summary>
        /// Force HVAC_GS1 to be the starting screen.
        /// </summary>
        public void SetFirstScreen()
        {
            currentScreenIndex = ScreensEnum.HVAC_GS1;
            backStack.Clear();
        }

        /// <summary>
        /// Sets the previously selected days of week.
        /// </summary>
        public List<DayOfWeek> SelectedDaysOfWeek
        {
            set
            {
                if (value == null)
                {
                    selectedDaysOfWeek.Clear();
                }
                else
                {
                    if (value.Count >
                        PublicMethods.GetEnumLength<DayOfWeek>())
                    {
                        throw new ArgumentException("Week too long");
                    }

                    foreach (DayOfWeek dow in value)
                    {
                        if (PublicMethods.IsValidEnumField<DayOfWeek>(dow)
                            == false)
                        {
                            throw new ArgumentException("Invalid day: " +
                                                        (int)dow);
                        }
                    }

                    selectedDaysOfWeek = value;
                }
            }
        }

        #endregion Public Interface

        #region Private Data-Structure

        /// <summary>
        /// Enum that describes all the specific screens.
        /// </summary>
        private static enum ScreensEnum
        {
            HVAC_GS1 = 0,
            HVAC_GS2,
            HVAC_GS3,
            HVAC_GS4,
            HVAC_GS5,
            HVAC_GS6,
            HVAC_GS7,
            HVAC_GS8,
            HVAC_GS9,
        }

        /// <summary>
        /// Data-Structure class that stores all info from each screen.
        /// </summary>
        private class ScreenData
        {
            public bool HasButtons;
            public string TopLabel;
            public string ControlTitle;
            public DateTime ControlDateTime;
            public bool status;
        }

        public static void hello() {

        }

    }
}
