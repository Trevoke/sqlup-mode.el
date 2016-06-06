Feature: Upcasing as I type
  In order to type proper SQL
  As a user
  I want to use sqlup-mode

  Scenario: Upcase a normal SQL keyword after entering a space
    Given I turn on sqlup-mode
    And I insert "select"
    And I type " "
    Then I should see "SELECT "

  Scenario: Upcase a normal SQL keyword after entering a (
    Given I turn on sqlup-mode
    And I insert "count"
    And I type "("
    Then I should see "COUNT("

  Scenario: Upcase a normal redis keyword after entering a space
    Given I turn on redis-mode
    And I turn on sqlup-mode
    And I insert "addslots"
    And I type " "
    Then I should see "ADDSLOTS "
