module LordOfTheRings.Quest.Cards.MidwintersCrux (midwintersCrux) where

import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum.Helpers (
  flipThaurdir,
  getThaurdir,
  isThaurdir,
  shadowCardsInPlay,
  thaurdirCaptain,
 )

newtype MidwintersCrux = MidwintersCrux QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

midwintersCrux :: QuestCard MidwintersCrux
midwintersCrux = quest MidwintersCrux Cards.midwintersCrux

-- | Thaurdir cannot leave the staging area, is considered to be engaged with each player,
-- and attacks each player in turn during the combat phase (deal and discard a new shadow
-- card for each attack). He stays in the staging area by his own card.
-- While Midwinter's Crux has 15 or more progress on it, Thaurdir loses indestructible.
instance HasModifiersFor MidwintersCrux where
  getModifiersFor (MidwintersCrux a) = when a.flipped do
    getThaurdir >>= traverse_ \t ->
      modified a (EnemyTarget t) $ EngagedWithEachPlayer AnyPlayer : [LoseKeyword Indestructible | a.progress >= 15]

-- | At the end of the round, if Thaurdir is Captain side faceup, flip him.
instance HasAbilities MidwintersCrux where
  getAbilities (MidwintersCrux a) = [restricted a 1 (EnemyExists thaurdirCaptain) $ forced RoundEndsWindow | a.flipped]

-- | When Revealed: Raise each player's threat by X, where X is the number of shadow cards in
-- play. If Thaurdir is Captain side faceup, flip him.
-- You cannot defeat this stage while Thaurdir is in play: its progress piles up, never
-- defeating it. If Thaurdir is defeated, you win the game.
instance RunMessage MidwintersCrux where
  runMessage msg q@(MidwintersCrux attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      MidwintersCrux <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      x <- shadowCardsInPlay
      eachPlayer \p -> encounterRaisesThreat p x
      captain <- selectAny thaurdirCaptain
      when captain flipThaurdir
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      flipThaurdir
      pure q
    -- with 15 progress he loses indestructible, and the damage he has may destroy him
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      let attrs' = attrs & tokensL %~ addTokens Progress n
      when (attrs'.progress >= 15) $ getThaurdir >>= traverse_ \t -> push $ CheckDefeated (toSource attrs) (EnemyTarget t)
      pure $ MidwintersCrux attrs'
    Destroy _ (EnemyTarget eid) -> do
      thaurdir <- isThaurdir eid
      when thaurdir winGame
      MidwintersCrux <$> runMessage msg attrs
    _ -> MidwintersCrux <$> runMessage msg attrs
