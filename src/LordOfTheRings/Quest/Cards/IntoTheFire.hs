module LordOfTheRings.Quest.Cards.IntoTheFire (intoTheFire) where

import LordOfTheRings.Card (cdKeywords, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (getRiddleStage, gollumEnemy)

newtype IntoTheFire = IntoTheFire QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoTheFire :: QuestCard IntoTheFire
intoTheFire = quest IntoTheFire Cards.intoTheFire

-- | Damage from undefended attacks made by Gollum must be placed on Bilbo Baggins.
-- (All riddle effects are ignored: see 'riddlesIgnored'.)
instance HasModifiersFor IntoTheFire where
  getModifiersFor (IntoTheFire a) = when a.flipped do
    gollums <- select gollumEnemy
    getBilbo >>= traverse_ \b -> for_ gollums \g -> modified a g [UndefendedDamageTo (toTarget b)]

{- | When Revealed: The first player gains control of Bilbo Baggins. Reveal 1 encounter card
per player and add it to the staging area.

Gollum engages the first player: kept so as long as the stage is in play, whoever holds
the first player token. Treachery cards gain surge: each one revealed reveals another
once it has resolved.

Stage 2B is complete as the players advance from stage 1B, and it leaves the riddle area
with Gollum and Bilbo Baggins as this stage turns over.
-}
instance RunMessage IntoTheFire where
  runMessage msg q@(IntoTheFire attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      traverse_ (push . LeaveQuestStage) =<< getRiddleStage
      askToResolveWhenRevealed attrs
      IntoTheFire <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      getBilbo >>= traverse_ \b -> push $ ChangeHeroController b lead
      revealEncounterCards . length =<< getRemainingPlayers
      pure q
    CheckWindows ws | attrs.flipped -> do
      for_ [tid | Window When (Revealed (TreacheryTarget tid)) <- ws] \tid -> do
        printed <- getsGame (maybe False ((Surge `member`) . cdKeywords . toCardDef) . lookup tid . (.entities.treacheries))
        unless printed $ revealEncounterCardAfter (TreacheryTarget tid)
      lead <- getLead
      gollums <- select gollumEnemy
      for_ gollums \g -> do
        e <- getEnemy g
        unless (e.placement == EngagedWith lead) $ engageEnemy lead g
      IntoTheFire <$> runMessage msg attrs
    _ -> IntoTheFire <$> runMessage msg attrs
