module LordOfTheRings.Enemy.Cards.TheWitchKingOsgiliath (theWitchKingOsgiliath) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype TheWitchKingOsgiliath = TheWitchKingOsgiliath EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

theWitchKingOsgiliath :: EnemyCard TheWitchKingOsgiliath
theWitchKingOsgiliath = enemy TheWitchKingOsgiliath Cards.theWitchKingOsgiliath

instance HasModifiersFor TheWitchKingOsgiliath where
  getModifiersFor (TheWitchKingOsgiliath a) = do
    modifySelf a [CannotPlayAttachmentsOn]
    when (a.placement == StagingArea) do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> modified a c [WillpowerModifier (-1)]

instance HasAbilities TheWitchKingOsgiliath where
  getAbilities (TheWitchKingOsgiliath a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage TheWitchKingOsgiliath where
  runMessage msg e@(TheWitchKingOsgiliath attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (headMay [pid | Window _ (EnemyAttacking eid pid) <- ws, eid == attrs.id]) \pid ->
        chooseOneM pid do
          questionLabeled "theWitchKingOsgiliath.choose"
          causedBy attrs
          labeled "theWitchKingOsgiliath.return" $ placeEnemy attrs.id StagingArea
          labeled "theWitchKingOsgiliath.threat" $ encounterRaisesThreat pid 3
      pure e
    _ -> TheWitchKingOsgiliath <$> runMessage msg attrs
