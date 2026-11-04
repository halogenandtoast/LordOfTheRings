module LordOfTheRings.Location.Cards.PalaceRuins (palaceRuins) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Undead))

newtype PalaceRuins = PalaceRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

palaceRuins :: LocationCard PalaceRuins
palaceRuins = location PalaceRuins Cards.palaceRuins

-- | Action: Discard the top X cards of your deck to reduce Palace Ruins' threat by X until
-- the end of the phase. Any player may trigger this action. (Limit once per phase.)
instance HasAbilities PalaceRuins where
  getAbilities (PalaceRuins a) = [anyPlayer $ limited (GroupLimit PerPhase 1) $ mkAbility a 1 $ ActionAbility Free]

-- | Shadow: Discard 1 card from the top of your deck for each Undead enemy engaged with you.
instance RunMessage PalaceRuins where
  runMessage msg l@(PalaceRuins a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      -- any X the deck holds; its threat goes no lower than 0
      deck <- length . (.deck) <$> getPlayer pid
      chooseAmountM pid "palaceRuins.amount" a 1 deck \x -> do
        discardTopOfDeck pid x
        phaseModifier a a (ThreatModifier (-x))
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      undead <- length <$> select (EnemyWithTrait Undead <> EnemyEngagedWith (PlayerWithId pid))
      when (undead > 0) $ discardTopOfDeck pid undead
      pure l
    _ -> PalaceRuins <$> runMessage msg a
