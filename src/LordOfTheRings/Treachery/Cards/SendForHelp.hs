module LordOfTheRings.Treachery.Cards.SendForHelp (
  sendForHelpLeadership,
  sendForHelpTactics,
  sendForHelpSpirit,
  sendForHelpLore,
) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (Card (PlayerCard), EncounterCard (..), genPlayerCard, toCardCode, toCardDef)
import LordOfTheRings.Card.Code (CardCode (..))
import LordOfTheRings.Hero (allHeroes)
import LordOfTheRings.Phase (Phase (..))
import LordOfTheRings.Player.Runner (canPlayAtNoCost)
import LordOfTheRings.Helpers.Campaign (campaignUpgraded)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

-- | Send for Help, a boon of the Dream-chaser campaign printed with an encounter card's
-- text: set aside, Gavin shuffles one into the encounter deck.
newtype SendForHelp = SendForHelp TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sendForHelpLeadership :: TreacheryCard SendForHelp
sendForHelpLeadership = treachery SendForHelp Cards.sendForHelpLeadership

sendForHelpTactics :: TreacheryCard SendForHelp
sendForHelpTactics = treachery SendForHelp Cards.sendForHelpTactics

sendForHelpSpirit :: TreacheryCard SendForHelp
sendForHelpSpirit = treachery SendForHelp Cards.sendForHelpSpirit

sendForHelpLore :: TreacheryCard SendForHelp
sendForHelpLore = treachery SendForHelp Cards.sendForHelpLore

{- | Surge. Each: Remove Send for Help from the game. Shadow: Resolve this card's when
revealed effect.
Leadership: When Revealed: Choose a player to search the top 10 cards of their deck for a
card, draw it or play it at no cost, and shuffle their deck. If upgraded, repeat this
effect.
Tactics: When Revealed: Search the players' collections for a hero and put it into play
under any player's control. If upgraded, repeat this effect. At the end of the round, or if
those heroes leave play, remove them from the game.
Spirit: When Revealed: Choose one: resource, planning, combat, or refresh. After the
current phase, there is an additional instance of the chosen phase. If upgraded, choose
two instead (resolve them in any order).
Lore: When Revealed: Search the players' collections for Gandalf ally (Core Set 73) and
put him into play under any player's control, ignoring uniqueness. If upgraded, that
player chooses up to 3 of Gandalf's options instead of 1.

The players' collections are every hero the engine knows that the unique rule lets into
play. A repeated effect runs again once the first has resolved, its search made afresh.
-}
instance RunMessage SendForHelp where
  runMessage msg t@(SendForHelp attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> resolve >> pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> resolve >> pure t
    DoStep n inner@(WhenRevealed (isTarget attrs -> True)) | n > 0 -> do
      lead <- getLead
      if toCardCode attrs == toCardCode Cards.sendForHelpLeadership
        then choosePlayerM lead attrs \pid -> do
          deck <- take 10 . (.deck) <$> getPlayer pid
          chooseOneM pid do
            questionLabeled "sendForHelp.search"
            causedBy attrs
            showing deck
            for_ [pc | PlayerCard pc <- deck] \pc -> targeting (CardIdTarget pc.id) do
              playable <- canPlayAtNoCost (PlayerCard pc)
              chooseOneM pid do
                questionLabeled "sendForHelp.drawOrPlay"
                causedBy attrs
                showing [PlayerCard pc]
                labeled "sendForHelp.draw" $ takeFromDeck pid (PlayerCard pc)
                when playable $ labeled "sendForHelp.play" $ playFromDeckAtNoCost pid pc
            labeled "sendForHelp.nothing" $ pure ()
          shufflePlayerDeck pid
        else choosePlayerM lead attrs \pid -> do
          collection <- flip filterM (Map.keys allHeroes) \code -> passesUnique . PlayerCard =<< genPlayerCard (toCardDef code) (Just pid)
          heroes <- traverse (\code -> genPlayerCard (toCardDef code) (Just pid)) collection
          chooseOneM pid do
            questionLabeled "sendForHelp.hero"
            causedBy attrs
            showing (map PlayerCard heroes)
            for_ heroes \card -> targeting (CardIdTarget card.id) do
              roundModifiers attrs (CardIdTarget card.id) [RemoveFromGameAtEndOfRound, RemoveFromGameIfLeavesPlay]
              putHeroIntoPlay pid card
      doStep (n - 1) inner
      pure t
    Discard (isTarget attrs -> True) -> do
      t' <- SendForHelp <$> runMessage msg attrs
      removeFromEncounterDiscard (MkEncounterCard attrs.cardId (toCardCode attrs))
      pure t'
    _ -> SendForHelp <$> runMessage msg attrs
   where
    resolve = do
      lead <- getLead
      upgraded <- campaignUpgraded (toCardCode attrs)
      case unCardCode (toCardCode attrs) of
        "DCC213" -> choosePhases lead [] (if upgraded then 2 else 1)
        "DCC214" -> choosePlayerM lead attrs \pid -> do
          card <- genPlayerCard Allies.gandalf (Just pid)
          -- before he enters play, for his response to it to read
          when upgraded $ roundModifier attrs (CardIdTarget card.id) (ChoosesUpToOptions 3)
          putAllyIntoPlayUnder pid card
        _ -> doStep (if upgraded then 2 else 1) (WhenRevealed $ toTarget attrs)
    -- the phases chosen, each once, resolved in the order they are chosen
    choosePhases :: ReverseQueue m => PlayerId -> [Phase] -> Int -> m ()
    choosePhases lead chosen n
      | n <= 0 = additionalPhases chosen
      | otherwise = chooseOneM lead do
          questionLabeled "sendForHelp.phase"
          causedBy attrs
          for_ [(ResourcePhase, "resource"), (PlanningPhase, "planning"), (CombatPhase, "combat"), (RefreshPhase, "refresh")] \(p, key) ->
            unless (p `elem` chosen) $ labeled ("sendForHelp." <> key) $ choosePhases lead (chosen <> [p]) (n - 1)
