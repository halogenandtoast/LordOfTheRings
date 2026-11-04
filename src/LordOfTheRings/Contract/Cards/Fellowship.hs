module LordOfTheRings.Contract.Cards.Fellowship (fellowship) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Contract.CardDefs qualified as Cards
import LordOfTheRings.Contract.Import.Lifted

newtype Fellowship = Fellowship ContractAttrs
  deriving anyclass IsContract
  deriving newtype (Show, Eq, ToJSON, Entity)

fellowship :: ContractCard Fellowship
fellowship = contract Fellowship Cards.fellowship

{- | Side A: You cannot play non-unique allies or put non-unique allies into play.
Side B: You cannot play allies or put allies into play. Each character you control gets
+1 willpower, +1 attack, and +1 defense.
-}
instance HasModifiersFor Fellowship where
  getModifiersFor (Fellowship a) = do
    let you = PlayerWithId a.owner
        barred = if a.flipped then CardWithType AllyType else CardWithType AllyType <> NotCard UniqueCard
    modified a (PlayerTarget a.owner) [CannotPlay barred, CannotPutIntoPlay barred]
    when a.flipped do
      heroes <- select (HeroControlledBy you)
      allies <- select (AllyControlledBy you)
      for_ (map toTarget heroes <> map toTarget allies) \c ->
        modified a c [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]

{- | Side A: Forced: When you control exactly 9 unique characters, flip this card over.
Side B: Forced: After a character you control leaves play, flip this card over.

The count changes only as a character comes under your control, so side A checks it then.
-}
instance HasAbilities Fellowship where
  getAbilities (Fellowship a)
    | a.flipped = [mkAbility a 2 $ forced $ ControlledCharacterLeftPlayWindow After a.owner]
    | otherwise =
        [ restricted a 1 (CharactersAtLeast 9 heroes allies <> NotCriterion (CharactersAtLeast 10 heroes allies))
            $ forced
            $ CharacterEnteredPlayUnderWindow After a.owner
        ]
   where
    heroes = HeroControlledBy (PlayerWithId a.owner)
    allies = AllyControlledBy (PlayerWithId a.owner) <> UniqueAlly

instance RunMessage Fellowship where
  runMessage msg c@(Fellowship attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) n _ | n `elem` [1, 2] -> do
      flipOver attrs
      pure c
    _ -> Fellowship <$> runMessage msg attrs
