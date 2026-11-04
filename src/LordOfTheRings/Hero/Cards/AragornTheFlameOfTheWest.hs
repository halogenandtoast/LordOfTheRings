module LordOfTheRings.Hero.Cards.AragornTheFlameOfTheWest (aragornTheFlameOfTheWest) where

import LordOfTheRings.Helpers.Fellowship (leavesPlay)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Trait (Trait (Artifact))

newtype AragornTheFlameOfTheWest = AragornTheFlameOfTheWest HeroAttrs
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Aragorn: his FirstPlayerGainsControl keyword.
aragornTheFlameOfTheWest :: HeroCard AragornTheFlameOfTheWest
aragornTheFlameOfTheWest = hero AragornTheFlameOfTheWest Cards.aragornTheFlameOfTheWest

-- | While Aragorn has an Artifact attached, he gains sentinel and does not exhaust to quest.
instance HasModifiersFor AragornTheFlameOfTheWest where
  getModifiersFor (AragornTheFlameOfTheWest a) = do
    artifact <- selectAny (HeroWithId a.id <> HeroWithAttached Artifact)
    when artifact $ modifySelf a [GainKeyword Sentinel, DoesNotExhaustToCommit]

-- | If Aragorn leaves play, the players lose the game.
instance RunMessage AragornTheFlameOfTheWest where
  runMessage msg h@(AragornTheFlameOfTheWest attrs) = case msg of
    _ | leavesPlay (toTarget attrs) msg -> do
      gameOver Lost
      pure h
    _ -> AragornTheFlameOfTheWest <$> runMessage msg attrs
