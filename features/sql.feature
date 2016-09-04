Feature: Upcasing SQL as I type
  In order to type proper SQL
  As a user
  I want to use sqlup-mode

  Background:
    Given the buffer is empty
    And I turn on sql-mode
    And I turn on sqlup-mode

  Scenario: Upcase a normal SQL keyword after entering a space
    Given I type "select "
    Then I should see "SELECT "

  Scenario: Upcase two keywords when typing
    Given I type "select * from -- comment"
    And I type " "
    Then I should see "SELECT * FROM -- comment"

  Scenario: Upcase a normal SQL keyword after entering a (
    Given I type "count("
    Then I should see "COUNT("

  Scenario: SQL keyword with underscore is upcased
    Given I type "character_set_name "
    Then I should see "CHARACTER_SET_NAME "

  Scenario: keywords in strings and comments do not get upcased
    Given I type "select count(*) 'select' -- select "
    Then I should see "SELECT COUNT(*) 'select' -- select "

  Scenario: Normal strings with _ ending in a keyword are not upcased
    When I type "select peer_group "
    Then I should see "SELECT peer_group "

  Scenario: Typing newlines should upcase strings
    When I type "select"
    When I type a newline
    Then I should see "SELECT"
