module LordOfTheRings.Quest.Cards.TroubleInBree (troubleInBree) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TroubleInBree = TroubleInBree QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

troubleInBree :: QuestCard TroubleInBree
troubleInBree = quest TroubleInBree Cards.troubleInBree

{- | Setup: Set all copies of Ringwraith, The Witch-king, Midgewater, and Weathertop aside,
out of play. Add The Prancing Pony and Bill Ferny to the staging area. Shuffle the
encounter deck. (The scenario set the others aside as it gathered the encounter deck.)

The players cannot advance while The Prancing Pony is in play.
-}
instance RunMessage TroubleInBree where
  runMessage msg q@(TroubleInBree attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ [Locations.thePrancingPony, Enemies.billFerny] \def ->
        for_ (find (cardMatches $ cardIs def) deck) \c -> do
          removeFromEncounterDeck c
          addToStaging c
      shuffleEncounterDeck
      TroubleInBree <$> runMessage msg attrs
    _ -> TroubleInBree <$> runConditionalStage (selectNone pony) msg attrs
   where
    pony = LocationWithCardCode (toCardCode Locations.thePrancingPony)
