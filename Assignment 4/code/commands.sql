SHOW TABLES LIKE 'Test';

CREATE TABLE Users (
    username varchar(255) UNIQUE,
    password varchar(255),
    uid varchar(255) UNIQUE
);

CREATE TABLE Tweets (
    tweet_id INT AUTO_INCREMENT,
    uid varchar(255),
    tweet TEXT
);

CREATE TABLE uid_subscribers (uid varchar(255));

CREATE TABLE uid_subscribed_to (
    subscribed_to varchar(255),
    tweet_id INT
);

CREATE TABLE hashtag_name_subscribers(uid varchar(255));

CREATE TABLE hashtag_name_tweets(tweet_id INT);

CREATE TABLE mention_name_subscribers(uid varchar(255));

CREATE TABLE mention_name_tweets(tweet_id INT);
