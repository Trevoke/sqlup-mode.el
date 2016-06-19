Feature: Upcasing SQL as I type
  In order to type proper SQL
  As a user
  I want to use sqlup-mode

  Background:
    Given I turn on sqlup-mode
    And the buffer is empty

  Scenario: Upcase a normal SQL keyword after entering a space
    Given I type "select "
    Then I should see "SELECT "

  Scenario: Upcase a normal SQL keyword after entering a (
    Given I type "count("
    Then I should see "COUNT("

  Scenario: Upcase two keywords when typing
    Given I type "select count("
    Then I should see "SELECT COUNT("

 Scenario: Upcase a normal SQL keyword in a postgres-execute eval string
    Given I start an action chain
    And I press "M-x"
    And I type "sql-highlight-postgres-keywords"
    And I execute the action chain
    And I type "execute 'select "
    Then I should see "EXECUTE 'SELECT "

 Scenario: Upcase a normal SQL keyword in a postgres-format eval string
    Given I start an action chain
    And I press "M-x"
    And I type "sql-highlight-postgres-keywords"
    And I execute the action chain
    And I type "execute format('select "
    Then I should see "EXECUTE format('SELECT "

 Scenario: Normal strings are never upcased
    Given I start an action chain
    And I press "M-x"
    And I type "sql-highlight-postgres-keywords"
    And I execute the action chain
    And I type "select 'case "
    Then I should see "SELECT 'case "
