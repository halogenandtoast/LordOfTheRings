module LordOfTheRings.Hero.Cards.Rossiel (rossiel) where

import LordOfTheRings.Card (CardType (EnemyType, LocationType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait)

newtype Rossiel = Rossiel HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, Entity)

rossiel :: HeroCard Rossiel
rossiel = hero Rossiel Cards.rossiel

-- | The traits of the cards of the type in the victory display.
displayedTraits :: HasGame m => CardType -> m (Set Trait)
displayedTraits t =
  getsGame
    $ foldMap cdCardTraits
    . filter ((== t) . cdCardType)
    . map toCardDef
    . (.victoryDisplay)
    . toAttrs
    . gameScenario

sharesTrait :: Set Trait -> Set Trait -> Bool
sharesTrait shown = any (`member` shown)

-- | If the active location shares a Trait with a location in the victory display, Rossiel
-- gets +2 willpower.
instance HasModifiersFor Rossiel where
  getModifiersFor (Rossiel a) = do
    shown <- displayedTraits LocationType
    active <- traverse getLocation =<< select IsActiveLocation
    when (any (sharesTrait shown . cdCardTraits . toCardDef) active) $ modifySelf a [WillpowerModifier 2]

-- | If the attacking enemy shares a Trait with an enemy in the victory display, Rossiel
-- gets +2 defense: read as she is declared its defender.
instance HasAbilities Rossiel where
  getAbilities (Rossiel a) = [mkAbility a 1 $ silent $ DefenderDeclaredWindow After (Just $ toTarget a)]

instance RunMessage Rossiel where
  runMessage msg h@(Rossiel attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      shown <- displayedTraits EnemyType
      for_ [eid | Window _ (DeclaredAsDefender _ eid) <- windows] \eid -> do
        traits <- cdCardTraits . toCardDef <$> getEnemy eid
        when (sharesTrait shown traits) $ attackModifier attrs attrs (DefenseModifier 2)
      pure h
    _ -> Rossiel <$> runMessage msg attrs
