module LordOfTheRings.Hero.Cards.BardTheBowman (bardTheBowman) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype BardTheBowman = BardTheBowman HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged, on its card def.
bardTheBowman :: HeroCard BardTheBowman
bardTheBowman = hero BardTheBowman Cards.bardTheBowman

{- | When Bard the Bowman makes a ranged attack, the enemy he attacks gets -2 [defense]
until the end of the phase.

A ranged attack is one on an enemy engaged with another player (FAQ 1.9, "What counts as a
'ranged' attack?"), or one a card names so, as Great Yew Bow's on an enemy in the staging
area: an enemy not engaged with Bard's controller, as Rivendell Bow reads it.
-}
instance HasAbilities BardTheBowman where
  getAbilities (BardTheBowman a) =
    [ mkAbility a 1
        $ silent
        $ CharacterAttackerDeclaredWindow After (HeroWithId a.id) (not_ AnyAlly) (not_ $ EnemyEngagedWith $ PlayerWithId a.heroOwner)
    ]

instance RunMessage BardTheBowman where
  runMessage msg h@(BardTheBowman attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (DeclaredAsAttacker _ eid) <- windows]) \eid ->
        phaseModifier attrs eid (DefenseModifier (-2))
      pure h
    _ -> BardTheBowman <$> runMessage msg attrs
