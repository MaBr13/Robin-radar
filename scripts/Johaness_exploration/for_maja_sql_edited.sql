-- FUNCTION TO CALCULATE SUNRISE SUNSET USING COORDINATES AND TIME 

-- rr_user is a schema that I created. Maybe you have a schema of your own in which you can create functions? 
-- Substitute the rr_user by your schema name and run in pgadmin/postgresql. 

CREATE OR REPLACE FUNCTION public.force_range(
  v DOUBLE PRECISION,
  max DOUBLE PRECISION
) RETURNS DOUBLE PRECISION AS $$
  BEGIN
IF v < 0 THEN
RETURN v + max;
ELSEIF v >= max THEN
return v - max;
END IF;

return v;
END; $$
  LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION public.rise_set_time(
  latitude DOUBLE PRECISION,
  longitude DOUBLE PRECISION,
  isRiseTime BOOL,
  as_of TIMESTAMPTZ,
  zenith DOUBLE PRECISION DEFAULT 90.8
)
RETURNS TIMESTAMPTZ AS $$
  DECLARE as_of_utc TIMESTAMPTZ;
DECLARE as_of_year INT;
DECLARE as_of_month INT;
DECLARE as_of_day INT;

DECLARE N1 INT;
DECLARE N2 INT;
DECLARE N3 INT;
DECLARE N INT;

DECLARE longitude_hour DOUBLE PRECISION;
DECLARE M DOUBLE PRECISION;

DECLARE t DOUBLE PRECISION;
DECLARE L DOUBLE PRECISION;
DECLARE RA DOUBLE PRECISION;

DECLARE Lquadrant INT;
DECLARE RAquadrant INT;
DECLARE sinDec DOUBLE PRECISION;
DECLARE cosDec DOUBLE PRECISION;
DECLARE cosH DOUBLE PRECISION;
DECLARE H DOUBLE PRECISION;
DECLARE UT DOUBLE PRECISION;

DECLARE hr INT;
DECLARE min INT;
BEGIN
as_of_utc = as_of at time zone 'utc';
as_of_year = EXTRACT(YEAR FROM as_of_utc);
as_of_month = EXTRACT(MONTH FROM as_of_utc);
as_of_day = EXTRACT(DAY FROM as_of_utc);

-- 1. first calculate the day of the year
N1 = FLOOR(275.0 * as_of_month / 9.0);
N2 = FLOOR((as_of_month + 9) / 12.0);
N3 = (1 + FLOOR((as_of_year - 4 * FLOOR(as_of_year / 4.0) + 2) / 3.0));
N = N1 - (N2 * N3) + as_of_day - 30;

-- 2. convert the longitude to hour value and calculate an approximate time
longitude_hour = longitude / 15.0;

IF isRiseTime THEN
t = N + ((6 - longitude_hour) / 24.);
ELSE
t = N + ((18 - longitude_hour) / 24.);
END IF;

-- 3. calculate the Sun's mean anomaly
    M = (0.9856 * t) - 3.289;

    -- 4. calculate the Sun's true longitude
L = M + (1.916 * SIN(RADIANS(M))) + (0.020 * SIN(RADIANS(2 * M))) + 282.634;
-- NOTE: L adjusted into the range [0,360)
L = public.force_range(L, 360.0);

-- 5a. calculate the Sun's right ascension
    RA = (1/RADIANS(1)) * ATAN(0.91764 * TAN(RADIANS(L)));
    RA = public.force_range( RA, 360 );  -- NOTE: RA adjusted into the range [0,360);

    -- 5b. right ascension value needs to be in the same quadrant as L
    Lquadrant = FLOOR(L/90.) * 90;
    RAquadrant = FLOOR(RA/90.) * 90;
    RA = RA + (Lquadrant - RAquadrant);

    -- 5c. right ascension value needs to be converted into hours
    RA = RA / 15.0;

    -- 6. calculate the Sun's declination
sinDec = 0.39782 * SIN(RADIANS(L));
cosDec = COS(ASIN(sinDec));

-- 7a. calculate the Sun's local hour angle
    cosH = (COS(RADIANS(zenith)) - (sinDec * SIN(RADIANS(latitude)))) / (cosDec * COS(RADIANS(latitude)));

    IF cosH > 1 THEN
        RAISE NOTICE 'The sun never rises on this location on the specified date';
        RETURN NULL;
    END IF;

    IF cosH < -1 THEN
        RAISE NOTICE 'The sun never sets on this location on the specified date';
        RETURN NULL;
    END IF;

    -- 7b. finish calculating H and convert into hours
    IF isRiseTime THEN
        H = 360 - (1/RADIANS(1)) * ACOS(cosH);
    ELSE
        H = (1/RADIANS(1)) * ACOS(cosH);
    END IF;

    H = H / 15.0;

    -- calculate local mean time of rising/setting
    T = H + RA - (0.06571 * t) - 6.622;

    -- 9. adjust back to UTC
    UT = T - longitude_hour;
    UT = public.force_range( UT, 24);  -- UTC time in decimal format (e.g. 23.23)

    -- 10. Return
    hr = public.force_range(UT::INT, 24);
    min = ROUND((UT - UT::INT) * 60);

--     Enable for debugging purposes:
--     RAISE NOTICE 'as_of_utc: %', as_of_utc;
--     RAISE NOTICE 'as_of_year: %', as_of_year;
--     RAISE NOTICE 'as_of_month: %', as_of_month;
--     RAISE NOTICE 'as_of_day: %', as_of_day;
--     RAISE NOTICE 'N1: %', N1;
--     RAISE NOTICE 'N2: %', N2;
--     RAISE NOTICE 'N3: %', N3;
--     RAISE NOTICE 'N: %', N;
--     RAISE NOTICE 'longitude_hour: %', longitude_hour;
--     RAISE NOTICE 'M: %', M;
--     RAISE NOTICE 't: %', t;
--     RAISE NOTICE 'L: %', L;
--     RAISE NOTICE 'RA: %', RA;
--     RAISE NOTICE 'Lquadrant: %', Lquadrant;
--     RAISE NOTICE 'RAquadrant: %', RAquadrant;
--     RAISE NOTICE 'sinDec: %', sinDec;
--     RAISE NOTICE 'cosDec: %', cosDec;
--     RAISE NOTICE 'cosH: %', cosH;
--     RAISE NOTICE 'H: %', H;
--     RAISE NOTICE 'UT: %', UT;
--     RAISE NOTICE 'hr: %', hr;
--     RAISE NOTICE 'min: %', min;

    return as_of_utc::DATE + (INTERVAL '1 hour' * hr) + (INTERVAL '1 minute' * min);
END; $$
LANGUAGE plpgsql IMMUTABLE;

COMMENT ON FUNCTION public.rise_set_time(double precision, double precision, boolean, timestamp with time zone, double precision)
    IS 'function to calculate sunrise and sunset. Example:
  SELECT public.rise_set_time(39.399872, -8.224454, TRUE, NOW()) AS rise,
public.rise_set_time(39.399872, -8.224454, FALSE, NOW()) AS set;';

