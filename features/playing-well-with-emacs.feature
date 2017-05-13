Feature: Playing well with emacs
  In order to be usable
  As a minor mode
  I want to be as non-surprising as possible

  Background:
    Given the buffer is empty

  Scenario: Toggling sqlup repeatedly
    Given I turn on sqlup-mode
    And I turn off sqlup-mode
    And I turn on sqlup-mode
    When I type "select *"
    Then I should see "SELECT *"

  Scenario: Not modifying text when enabling sqlup-mode
    Given I turn on ruby-mode
    When I type "end"
    And I turn on sqlup-mode
    Then I should see "end"

  Scenario: Not modifying text when enabling sqlup-mode
    Given I add sqlup-mode to sql-mode hook
    Given I turn on text-mode
    When I type "select"
    And I turn on sql-mode
    Then I should see "select"
