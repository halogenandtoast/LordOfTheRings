module LordOfTheRings.Ally.Cards.Thalion (thalion) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card, cdSphere, toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (InPlayArea))

newtype Thalion = Thalion AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

thalion :: AllyCard Thalion
thalion = ally Thalion Cards.thalion

-- | The side quests in the victory display.
sideQuestsShown :: HasGame m => m [Card]
sideQuestsShown = getsGame (filter (cardMatches sideQuestCard) . (.victoryDisplay) . toAttrs . gameScenario)

-- | As a hero, Thalion gains the resource icons of each sphere on a side quest in the
-- victory display.
instance HasModifiersFor Thalion where
  getModifiersFor (Thalion a) = when a.allyAsHero do
    spheres <- ordNub . mapMaybe (cdSphere . toCardDef) <$> sideQuestsShown
    modifySelf a (map GainResourceIcon spheres)

-- | While there is at least 1 side quest in the victory display, ready Thalion at the
-- beginning of the combat phase.
instance HasAbilities Thalion where
  getAbilities (Thalion a) =
    [restricted a 1 (CardInVictoryDisplay sideQuestCard) $ forced $ PhaseBeginsWindow After CombatPhase]

-- | While there are at least 3 side quests in the victory display, Thalion loses the ally
-- card type and gains the hero card type: as a side quest enters the victory display or
-- leaves it, he becomes the one or the other, keeping his state.
instance RunMessage Thalion where
  runMessage msg s@(Thalion a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    AllyBecomesHero _ -> Thalion <$> runMessage msg a
    HeroBecomesAlly _ -> Thalion <$> runMessage msg a
    _ -> do
      a' <- runMessage msg a
      hero' <- (>= 3) . length <$> sideQuestsShown
      when (inPlay a' && hero' /= a'.allyAsHero)
        $ if hero' then allyBecomesHero a' else heroBecomesAlly a'
      pure $ Thalion a'
   where
    inPlay x = case x.placement of
      InPlayArea _ -> True
      _ -> False
