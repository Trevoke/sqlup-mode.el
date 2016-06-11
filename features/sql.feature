Feature: Upcasing SQL as I type
  In order to type proper SQL
  As a user
  I want to use sqlup-mode

  Background:
    Given I turn on sqlup-mode
    And the buffer is empty

  Scenario: Upcase a normal SQL keyword after entering a space
    Given I insert "select"
    And I type " "
    Then I should see "SELECT "

  Scenario: Upcase a normal SQL keyword after entering a (
    Given I insert "count"
    And I type "("
    Then I should see "COUNT("

 Scenario: Upcase a normal SQL keyword in a postgres-execute eval string
    Given I start an action chain
    And I press "M-x"
    And I type "sql-highlight-postgres-keywords"
    And I execute the action chain
    And I insert "execute"
    And I type " "
    And I insert "'select"
    And I type " "
    Then I should see "EXECUTE 'SELECT "

 Scenario: Upcase a normal SQL keyword in a postgres-format eval string
    Given I start an action chain
    And I press "M-x"
    And I type "sql-highlight-postgres-keywords"
    And I execute the action chain
    And I insert "execute"
    And I type " "
    And I insert "format("
    And I insert "'select"
    And I type " "
    Then I should see "EXECUTE format('SELECT "

 Scenario: Normal strings are never upcased
    Given I start an action chain
    And I press "M-x"
    And I type "sql-highlight-postgres-keywords"
    And I execute the action chain
    And I insert "select"
    And I type " "
    And I insert "'case"
    And I type " "
    Then I should see "SELECT 'case "
